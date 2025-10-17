use std::{
    collections::HashMap,
    sync::{Arc, Weak},
};

use anyhow::{Context, Result, anyhow};
use imgui::{DrawData, DrawIdx, FontAtlasFlags, TextureId, internal::RawWrapper};
use smallvec::smallvec;
use vk::{
    command_buffer::PrimaryCommandBufferAbstract,
    pipeline::{
        Pipeline,
        graphics::vertex_input::{Vertex, VertexDefinition},
    },
    sync::GpuFuture,
};
use vulkano as vk;
use vulkano_util::context::VulkanoContext;

use self::shaders::DrawVert;

/// The renderer structure.
pub struct Renderer {
    /// The Vulkan context, holding our instance, device, queue, etc.
    context: Arc<VulkanoContext>,
    /// The format of the color buffer.
    color_format: vk::format::Format,

    /// The configuration of our render pass.
    render_pass: Arc<vk::render_pass::RenderPass>,
    /// Our graphics pipeline, specifying the settings and shaders to use
    /// to transform vertex attributes into pixels on the screen.
    pipeline: Arc<vk::pipeline::GraphicsPipeline>,
    command_buffer_allocator: Arc<vk::command_buffer::allocator::StandardCommandBufferAllocator>,

    /// The descriptor set allocator.
    descriptor_set_allocator: Arc<vk::descriptor_set::allocator::StandardDescriptorSetAllocator>,

    /// The descriptor set layout binding our texture image and sampler.
    sampler_descriptor_set_layout: Arc<vk::descriptor_set::layout::DescriptorSetLayout>,

    /// The textures that have been created.
    textures: HashMap<TextureId, Weak<Texture>>,

    /// Our buffer allocator.
    buffer_allocator:
        vk::buffer::allocator::SubbufferAllocator<vk::memory::allocator::StandardMemoryAllocator>,

    previous_frame: Option<Box<dyn GpuFuture>>,

    /// An integer identifying the last frame to be rendered.
    frame_id: u64,

    /// The next texture ID to be assigned.
    next_texture_id: usize,

    _font_texture: Arc<Texture>,
}

#[derive(Debug)]
pub struct Target {
    /// The window this target belongs to.
    window: Arc<winit::window::Window>,
    /// The surface to render to.
    surface: Arc<vk::swapchain::Surface>,
    /// The swapchain to use, and the buffers bound to that swapchain.
    framebuffers: Framebuffers,
}

pub struct Texture {
    pub id: TextureId,
    descriptor: Arc<vk::descriptor_set::DescriptorSet>,
}

pub struct Frame<'a> {
    pub renderer: &'a mut Renderer,
    pub inflight: Option<Box<dyn GpuFuture>>,
}

mod shaders {
    use vulkano::{buffer::BufferContents, pipeline::graphics::vertex_input::Vertex};
    use vulkano_shaders::shader;

    #[derive(BufferContents, Vertex, Clone, Copy, Debug)]
    #[repr(C)]
    pub struct DrawVert {
        #[format(R32G32_SFLOAT)]
        pub pos: [f32; 2],

        #[format(R32G32_SFLOAT)]
        pub uv: [f32; 2],

        #[format(R8G8B8A8_UNORM)]
        pub col: [u8; 4],
    }

    shader! {
        shaders: {
            vertex: {
                ty: "vertex",
                src: r"
                    #version 400
                    #extension GL_ARB_separate_shader_objects  : enable

                    layout(location=0) in vec2 pos;
                    layout(location=1) in vec2 uv;
                    layout(location=2) in vec4 col;

                    layout(push_constant) uniform Transform { vec2 scale; vec2 translate; } transform;

                    layout(location=0) out vec2 frag_uv;
                    layout(location=1) out vec4 frag_col;

                    void main() {
                        gl_Position = vec4(pos * transform.scale + transform.translate, 0, 1);
                        frag_uv = uv;
                        frag_col = col;
                    }
                "
            },
            fragment: {
                ty: "fragment",
                src: r"
                    #version 400
                    #extension GL_ARB_separate_shader_objects  : enable
                    #extension GL_ARB_shading_language_420pack : enable
                    
                    layout(location=0) in vec2 frag_uv;
                    layout(location=1) in vec4 frag_col;

                    layout(set=0, binding=0) uniform sampler2D tex;

                    layout(location=0) out vec4 out_col;

                    void main() {
                        out_col = frag_col * texture(tex, frag_uv);
                    }
                "
            }
        }
    }
}

impl Renderer {
    const FONT_TEXTURE_ID: TextureId = TextureId::new(0);

    pub fn context(&self) -> &VulkanoContext {
        &self.context
    }
    pub fn command_buffer_allocator(
        &self,
    ) -> &Arc<vk::command_buffer::allocator::StandardCommandBufferAllocator> {
        &self.command_buffer_allocator
    }

    pub fn begin(&mut self) -> Frame<'_> {
        if let Some(f) = self.previous_frame.as_mut() {
            f.cleanup_finished()
        }
        Frame {
            inflight: None,
            renderer: self,
        }
    }

    fn _render(
        &mut self,
        target: &mut Target,
        draw_data: &DrawData,
    ) -> Result<impl GpuFuture + use<>> {
        // Create a command buffer to store our rendering commands.
        let mut command_buffer = vk::command_buffer::AutoCommandBufferBuilder::primary(
            self.command_buffer_allocator.clone(),
            self.context.graphics_queue().queue_family_index(),
            vk::command_buffer::CommandBufferUsage::OneTimeSubmit,
        )?;

        // Acquire a framebuffer we can render into.. This will (re-)create the framebuffers and
        // swapchain if necessary. If all framebuffers are currently in use by the GPU, this will
        // block until one becomes available.
        let framebuffer = target.next(self)?;

        // Start the render pass by binding our framebuffer
        // and clearing the color attachment.
        command_buffer.begin_render_pass(
            vk::command_buffer::RenderPassBeginInfo {
                clear_values: vec![Some(vk::format::ClearValue::Float([0., 0., 0., 0.]))],
                ..vk::command_buffer::RenderPassBeginInfo::framebuffer(framebuffer.framebuffer)
            },
            vk::command_buffer::SubpassBeginInfo {
                contents: vk::command_buffer::SubpassContents::Inline,
                ..Default::default()
            },
        )?;

        // Configure our viewport dimensions.
        let dimensions = framebuffer.swapchain.image_extent();
        let dimensions = [dimensions[0] as f32, dimensions[1] as f32];
        command_buffer.set_viewport(
            0,
            smallvec![vk::pipeline::graphics::viewport::Viewport {
                offset: [0.0, 0.0],
                extent: dimensions,
                depth_range: 0.0..=1.0,
            }],
        )?;

        // Use our graphics pipeline.
        command_buffer.bind_pipeline_graphics(self.pipeline.clone())?;

        // Upload offset/scale push constants.
        // Map (x, y) to (-1, -1) and (x+w, y+h) to (1,  1)
        // (0, 0) => (-1 - x*scale, -1 - y*scale)
        // 1 unit in the X direction goes to 2/w units
        let x = draw_data.display_pos[0];
        let y = draw_data.display_pos[1];
        let w = draw_data.display_size[0];
        let h = draw_data.display_size[1];
        let scale = [2. / w, 2. / h];
        let translate = [-1. - x * scale[0], -1. - y * scale[1]];
        let transform = shaders::Transform { scale, translate };
        command_buffer.push_constants(self.pipeline.layout().clone(), 0, transform)?;

        // Draw our geometry.
        for list in draw_data.draw_lists() {
            // Upload vertex and index buffers.
            let vertex_data: &[DrawVert] = unsafe { list.transmute_vtx_buffer() };
            let vertex_buffer = self
                .buffer_allocator
                .allocate_slice(vertex_data.len() as u64)?;
            vertex_buffer.write()?.copy_from_slice(vertex_data);

            let index_data: &[DrawIdx] = list.idx_buffer();
            let index_buffer = self
                .buffer_allocator
                .allocate_slice(index_data.len() as u64)?;
            index_buffer.write()?.copy_from_slice(index_data);

            // Bind the buffers to the pipeline.
            command_buffer.bind_vertex_buffers(0, vertex_buffer)?;
            command_buffer.bind_index_buffer(index_buffer)?;

            // Execute the draw commands.
            for cmd in list.commands() {
                match cmd {
                    imgui::DrawCmd::Elements {
                        count,
                        cmd_params:
                            imgui::DrawCmdParams {
                                clip_rect,
                                texture_id,
                                vtx_offset,
                                idx_offset,
                            },
                    } => {
                        // Bind the texture.
                        let texture = self
                            .textures
                            .get(&texture_id)
                            .and_then(|t| t.upgrade())
                            .ok_or_else(|| anyhow!("No such texture"))?;

                        command_buffer.bind_descriptor_sets(
                            vk::pipeline::PipelineBindPoint::Graphics,
                            self.pipeline.layout().clone(),
                            0,
                            texture.descriptor.clone(),
                        )?;

                        // Configure the scissor.
                        let clip_min = [
                            (clip_rect[0] - draw_data.display_pos[0])
                                * draw_data.framebuffer_scale[0],
                            (clip_rect[1] - draw_data.display_pos[1])
                                * draw_data.framebuffer_scale[1],
                        ];
                        let clip_max = [
                            (clip_rect[2] - draw_data.display_pos[0])
                                * draw_data.framebuffer_scale[0],
                            (clip_rect[3] - draw_data.display_pos[1])
                                * draw_data.framebuffer_scale[1],
                        ];
                        let framebuffer_bounds = [
                            draw_data.display_size[0] * draw_data.framebuffer_scale[0],
                            draw_data.display_size[1] * draw_data.framebuffer_scale[1],
                        ];

                        let clip_min = [clip_min[0].max(0.) as u32, clip_min[1].max(0.) as u32];
                        let clip_max = [
                            clip_max[0].min(framebuffer_bounds[0]) as u32,
                            clip_max[1].min(framebuffer_bounds[1]) as u32,
                        ];

                        command_buffer.set_scissor(
                            0,
                            smallvec![vk::pipeline::graphics::viewport::Scissor {
                                offset: clip_min,
                                extent: clip_max,
                            }],
                        )?;

                        // Draw the thing.
                        unsafe {
                            command_buffer.draw_indexed(
                                count as u32,
                                1,
                                idx_offset as u32,
                                vtx_offset as i32,
                                0,
                            )?;
                        }
                    }
                    imgui::DrawCmd::ResetRenderState => {}
                    imgui::DrawCmd::RawCallback { callback, raw_cmd } => unsafe {
                        callback(list.raw(), raw_cmd);
                    },
                }
            }
        }

        // we're done rendering!
        command_buffer.end_render_pass(vk::command_buffer::SubpassEndInfo::default())?;

        self.frame_id += 1;

        // Submit the command buffer for execution after the next framebuffer is available, and the
        // previous frame finishes rendering.
        let ready = match std::mem::take(&mut self.previous_frame) {
            Some(prev) => framebuffer.ready.join(prev).boxed(),
            _ => framebuffer.ready.boxed(),
        };
        let inflight = command_buffer
            .build()?
            .execute_after(ready, self.context.graphics_queue().clone())?
            .then_swapchain_present(
                self.context.graphics_queue().clone(),
                vk::swapchain::SwapchainPresentInfo::swapchain_image_index(
                    framebuffer.swapchain.clone(),
                    framebuffer.index as u32,
                ),
            );
        inflight.flush()?;

        // Clean up any old textures.
        self.textures.retain(|_, t| t.strong_count() != 0);

        Ok(inflight)
    }

    /// Initializes all rendering resources.
    pub fn new(
        context: Arc<VulkanoContext>,
        surface: &Arc<vk::swapchain::Surface>,
        imgui: &mut imgui::Context,
    ) -> Result<Self> {
        // Choose a color and depth format.
        let color_format = context
            .device()
            .physical_device()
            .surface_formats(surface, Default::default())?
            .iter()
            .find(|(f, c)| {
                *c == vk::swapchain::ColorSpace::SrgbNonLinear
                    && [
                        vk::format::Format::R8G8B8A8_UNORM,
                        vk::format::Format::B8G8R8A8_UNORM,
                    ]
                    .contains(f)
            })
            .ok_or_else(|| anyhow!("no suitable color formats"))?
            .0;

        // Define our render pass.
        let render_pass = vulkano::single_pass_renderpass!(context.device().clone(),
            attachments: {
                // Clear the color buffer on load, and store it to memory so we can see it.
                color: {
                    format: color_format,
                    samples: 1,
                    load_op: Clear,
                    store_op: Store,
                },
            },
            pass: {
                color: [color],
                depth_stencil: {}
            }
        )?;

        // Create a buffer allocator.
        let buffer_allocator = vk::buffer::allocator::SubbufferAllocator::new(
            context.memory_allocator().clone(),
            vk::buffer::allocator::SubbufferAllocatorCreateInfo {
                buffer_usage: vk::buffer::BufferUsage::VERTEX_BUFFER
                    | vk::buffer::BufferUsage::INDEX_BUFFER,
                memory_type_filter: vk::memory::allocator::MemoryTypeFilter::PREFER_DEVICE
                    | vk::memory::allocator::MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                ..Default::default()
            },
        );

        // Now let's set up our descriptor sets.
        let descriptor_set_allocator = Arc::new(
            vk::descriptor_set::allocator::StandardDescriptorSetAllocator::new(
                context.device().clone(),
                vk::descriptor_set::allocator::StandardDescriptorSetAllocatorCreateInfo::default(),
            ),
        );

        // Create a command buffer so we can upload our font texture.
        let command_buffer_allocator = Arc::new(
            vk::command_buffer::allocator::StandardCommandBufferAllocator::new(
                context.device().clone(),
                Default::default(),
            ),
        );
        let mut command_buffer = vk::command_buffer::AutoCommandBufferBuilder::primary(
            command_buffer_allocator.clone(),
            context.graphics_queue().queue_family_index(),
            vk::command_buffer::CommandBufferUsage::OneTimeSubmit,
        )?;

        // Create an image buffer in GPU memory, and upload it to the GPU.
        imgui.fonts().flags.insert(FontAtlasFlags::NO_BAKED_LINES);
        let font_image = imgui.fonts().build_rgba32_texture();
        let font_staging_buffer = vk::buffer::Buffer::new_slice(
            context.memory_allocator().clone(),
            vk::buffer::BufferCreateInfo {
                usage: vk::buffer::BufferUsage::TRANSFER_SRC,
                ..Default::default()
            },
            vk::memory::allocator::AllocationCreateInfo {
                memory_type_filter: vk::memory::allocator::MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                ..Default::default()
            },
            font_image.data.len() as u64,
        )?;
        font_staging_buffer
            .write()
            .unwrap()
            .copy_from_slice(font_image.data);
        let font_image = vk::image::Image::new(
            context.memory_allocator().clone(),
            vk::image::ImageCreateInfo {
                format: vk::format::Format::R8G8B8A8_UNORM,
                view_formats: vec![vk::format::Format::R8G8B8A8_UNORM],
                extent: [font_image.width, font_image.height, 1],
                usage: vk::image::ImageUsage::TRANSFER_DST | vk::image::ImageUsage::SAMPLED,
                ..Default::default()
            },
            vk::memory::allocator::AllocationCreateInfo {
                memory_type_filter: vk::memory::allocator::MemoryTypeFilter::PREFER_DEVICE,
                ..Default::default()
            },
        )?;
        command_buffer.copy_buffer_to_image(
            vk::command_buffer::CopyBufferToImageInfo::buffer_image(
                font_staging_buffer,
                font_image.clone(),
            ),
        )?;

        let create_info = vk::image::view::ImageViewCreateInfo::from_image(&font_image);
        let font_image = vk::image::view::ImageView::new(font_image, create_info)?;

        // Create a sampler that describes how to access our image.
        let sampler = vk::image::sampler::Sampler::new(
            context.device().clone(),
            vk::image::sampler::SamplerCreateInfo {
                mag_filter: vk::image::sampler::Filter::Nearest,
                min_filter: vk::image::sampler::Filter::Nearest,
                ..Default::default()
            },
        )?;

        // Create a descriptor set binding our texture & sampler to the graphics pipeline.
        let sampler_descriptor_set_layout = vk::descriptor_set::layout::DescriptorSetLayout::new(
            context.device().clone(),
            vk::descriptor_set::layout::DescriptorSetLayoutCreateInfo {
                bindings: [(
                    0,
                    vk::descriptor_set::layout::DescriptorSetLayoutBinding {
                        stages: vk::shader::ShaderStage::Fragment.into(),
                        immutable_samplers: vec![sampler.clone()],
                        ..vk::descriptor_set::layout::DescriptorSetLayoutBinding::descriptor_type(
                            vk::descriptor_set::layout::DescriptorType::CombinedImageSampler,
                        )
                    },
                )]
                .into(),
                ..Default::default()
            },
        )?;

        let vs = shaders::load_vertex(context.device().clone())?
            .entry_point("main")
            .context("entry point not found")?;
        let fs = shaders::load_fragment(context.device().clone())?
            .entry_point("main")
            .context("entry point not found")?;
        let stages = smallvec![
            vulkano::pipeline::PipelineShaderStageCreateInfo::new(vs.clone()),
            vulkano::pipeline::PipelineShaderStageCreateInfo::new(fs),
        ];
        let mut layout =
            vk::pipeline::layout::PipelineDescriptorSetLayoutCreateInfo::from_stages(stages.iter());
        // Adjust the pipeline layout to fit our sampler.
        layout.set_layouts[0]
            .bindings
            .get_mut(&0)
            .unwrap()
            .immutable_samplers = vec![sampler.clone()];
        let layout = vk::pipeline::PipelineLayout::new(
            context.device().clone(),
            layout.into_pipeline_layout_create_info(context.device().clone())?,
        )?;

        // Define our graphics pipeline:
        let pipeline = vk::pipeline::GraphicsPipeline::new(
            context.device().clone(),
            None,
            vk::pipeline::graphics::GraphicsPipelineCreateInfo {
                stages,
                // Pass the inputs through our vertex shader
                vertex_input_state: Some(shaders::DrawVert::per_vertex().definition(&vs)?),
                rasterization_state: Some(Default::default()),
                input_assembly_state: Some(Default::default()),
                // Do not use depth or stencil testing.
                depth_stencil_state: None,
                // Enable alpha blending
                color_blend_state: Some(
                    vk::pipeline::graphics::color_blend::ColorBlendState::with_attachment_states(
                        1,
                        vk::pipeline::graphics::color_blend::ColorBlendAttachmentState {
                            blend: Some(
                                vk::pipeline::graphics::color_blend::AttachmentBlend::alpha(),
                            ),
                            ..Default::default()
                        },
                    ),
                ),
                multisample_state: Some(Default::default()),
                viewport_state: Some(Default::default()),
                // Enable dynamic scissoring
                dynamic_state: [
                    vk::pipeline::DynamicState::Viewport,
                    vk::pipeline::DynamicState::Scissor,
                ]
                .into_iter()
                .collect(),
                // Use the render pass we described above
                subpass: Some(
                    vk::pipeline::graphics::subpass::PipelineSubpassType::BeginRenderPass(
                        render_pass.clone().first_subpass(),
                    ),
                ),
                ..vk::pipeline::graphics::GraphicsPipelineCreateInfo::layout(layout)
            },
        )?;

        let font_texture = Self::_create_texture(
            Self::FONT_TEXTURE_ID,
            font_image,
            descriptor_set_allocator.clone(),
            sampler_descriptor_set_layout.clone(),
        )?;
        let textures = HashMap::from_iter([(Self::FONT_TEXTURE_ID, Arc::downgrade(&font_texture))]);

        // Submit our upload commands to the GPU.
        command_buffer
            .build()?
            .execute(context.graphics_queue().clone())?
            .then_signal_fence_and_flush()?
            .wait(None)?;

        let result = Renderer {
            previous_frame: None,

            context,

            color_format,
            render_pass,
            pipeline,
            command_buffer_allocator,

            buffer_allocator,

            descriptor_set_allocator,

            sampler_descriptor_set_layout,
            _font_texture: font_texture,
            textures,
            next_texture_id: Self::FONT_TEXTURE_ID.id() + 1,

            frame_id: 0,
        };

        Ok(result)
    }

    pub fn create_texture(
        &mut self,
        image_view: Arc<vk::image::view::ImageView>,
    ) -> Result<Arc<Texture>> {
        let id = TextureId::new(self.next_texture_id);
        let texture = Self::_create_texture(
            id,
            image_view,
            self.descriptor_set_allocator.clone(),
            self.sampler_descriptor_set_layout.clone(),
        )?;

        self.textures.insert(id, Arc::downgrade(&texture));
        self.next_texture_id += 1;
        Ok(texture)
    }

    fn _create_texture(
        id: TextureId,
        image_view: Arc<vk::image::view::ImageView>,
        descriptor_set_allocator: Arc<
            vk::descriptor_set::allocator::StandardDescriptorSetAllocator,
        >,
        sampler_descriptor_set_layout: Arc<vk::descriptor_set::layout::DescriptorSetLayout>,
    ) -> Result<Arc<Texture>> {
        Ok(Arc::new(Texture {
            id,
            descriptor: vk::descriptor_set::DescriptorSet::new(
                descriptor_set_allocator,
                sampler_descriptor_set_layout,
                [vk::descriptor_set::WriteDescriptorSet::image_view(
                    0, image_view,
                )],
                [],
            )?,
        }))
    }
}

impl Frame<'_> {
    pub fn render(&mut self, target: &mut Target, draw_data: &DrawData) -> Result<()> {
        let inflight = self.renderer._render(target, draw_data)?;
        self.inflight = match std::mem::take(&mut self.inflight) {
            Some(prev) => Some(prev.join(inflight).boxed()),
            _ => Some(inflight.boxed()),
        };
        Ok(())
    }

    pub fn wait(mut self) -> Result<()> {
        if let Some(inflight) = std::mem::take(&mut self.inflight) {
            inflight.then_signal_fence_and_flush()?.wait(None)?;
        }

        Ok(())
    }
}

impl Drop for Frame<'_> {
    fn drop(&mut self) {
        self.renderer.previous_frame = std::mem::take(&mut self.inflight);
    }
}

impl Target {
    /// Creates a new render target for the given surface.
    pub fn new(window: Arc<winit::window::Window>, surface: Arc<vk::swapchain::Surface>) -> Self {
        Self {
            window,
            surface,
            framebuffers: Framebuffers::Invalid {
                old_swapchain: None,
            },
        }
    }

    /// Marks the swapchain as invalid so it will be recreated before rendering the next frame.
    pub fn invalidate(&mut self) {
        self.framebuffers = match std::mem::take(&mut self.framebuffers) {
            Framebuffers::Valid { swapchain, .. } => Framebuffers::Invalid {
                old_swapchain: Some(swapchain),
            },
            invalid => invalid,
        }
    }

    /// Acquires the next framebuffer in the swapchain.
    /// Recreates the swapchain if necessary. If all framebuffers are currently in use by the GPU,
    /// blocks until one becomes available.
    fn next(&mut self, renderer: &Renderer) -> Result<Framebuffer> {
        loop {
            // Do we have a valid swapchain already?
            let (swapchain, framebuffers) = match &self.framebuffers {
                Framebuffers::Valid {
                    swapchain,
                    framebuffers,
                } => (swapchain.clone(), framebuffers), // Yes, use it.
                Framebuffers::Invalid { .. } => {
                    // No, create one.
                    let extent = self.window.inner_size();
                    let extent = [extent.width, extent.height];
                    self.create(renderer, extent)?;
                    continue;
                }
            };

            // Get the next framebuffer from the swapchain.
            let (index, suboptimal, ready) =
                vk::swapchain::acquire_next_image(swapchain.clone(), None)?;
            {
                let index = index as usize;
                let framebuffer = framebuffers[index].clone();
                if suboptimal {
                    // Vulkan told us that the surface has changed so that our swapchain is no
                    // longer a perfect match. We can still render with it, but we should
                    // recreate it next time for better results.
                    self.invalidate();
                }
                return Ok(Framebuffer {
                    swapchain,
                    framebuffer,
                    index,
                    ready,
                });
            }
        }
    }

    /// Creates or recreates the framebuffers and swapchain.
    pub fn create(&mut self, renderer: &Renderer, image_extent: [u32; 2]) -> Result<()> {
        // If we have an old swapchain, we can reuse its resources.
        let old_swapchain = match std::mem::take(&mut self.framebuffers) {
            Framebuffers::Valid { swapchain, .. } => Some(swapchain),
            Framebuffers::Invalid { old_swapchain } => old_swapchain,
        };

        let surface_caps = renderer
            .context
            .device()
            .physical_device()
            .surface_capabilities(&self.surface, Default::default())?;

        let mut min_image_count = 3;
        min_image_count = min_image_count.max(surface_caps.min_image_count);
        if let Some(max_image_count) = surface_caps.max_image_count {
            min_image_count = min_image_count.min(max_image_count);
        }

        let swapchain_info = vk::swapchain::SwapchainCreateInfo {
            min_image_count,
            image_format: renderer.color_format,
            image_color_space: vk::swapchain::ColorSpace::SrgbNonLinear,
            image_usage: vk::image::ImageUsage::COLOR_ATTACHMENT,
            image_extent,
            ..Default::default()
        };

        // Create a swapchain & color buffers for our surface.
        let (swapchain, color_buffers) = if let Some(old) = old_swapchain {
            old.recreate(swapchain_info)?
        } else {
            vk::swapchain::Swapchain::new(
                renderer.context.device().clone(),
                self.surface.clone(),
                swapchain_info,
            )?
        };

        // Bind the color buffers to create framebuffers.
        let framebuffers: Vec<_> = color_buffers
            .into_iter()
            .map(|color_buffer| {
                Ok(vk::render_pass::Framebuffer::new(
                    renderer.render_pass.clone(),
                    vk::render_pass::FramebufferCreateInfo {
                        attachments: vec![vk::image::view::ImageView::new_default(color_buffer)?],
                        ..Default::default()
                    },
                )?)
            })
            .collect::<Result<Vec<Arc<vk::render_pass::Framebuffer>>>>()?;

        self.framebuffers = Framebuffers::Valid {
            swapchain,
            framebuffers,
        };

        Ok(())
    }
}

/// The framebuffers & swapchain to use for rendering.
#[derive(Debug)]
enum Framebuffers {
    /// We do not have valid framebuffers.
    Invalid {
        old_swapchain: Option<Arc<vk::swapchain::Swapchain>>,
    },

    /// We have a valid swapchain with valid framebuffers.
    Valid {
        swapchain: Arc<vk::swapchain::Swapchain>,
        framebuffers: Vec<Arc<vk::render_pass::Framebuffer>>,
    },
}

/// A single framebuffer from the swapchain.
struct Framebuffer {
    /// The swapchain this framebuffer belongs to.
    pub swapchain: Arc<vk::swapchain::Swapchain>,

    /// The framebuffer itself.
    pub framebuffer: Arc<vk::render_pass::Framebuffer>,

    /// The index of the framebuffer within the swapchain.
    pub index: usize,

    /// The time at which the framebuffer will be available for us to render into.
    pub ready: vk::swapchain::SwapchainAcquireFuture,
}

impl Default for Framebuffers {
    fn default() -> Self {
        Self::Invalid {
            old_swapchain: None,
        }
    }
}
