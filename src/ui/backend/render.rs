use std::{
    collections::HashMap,
    num::NonZeroU64,
    sync::{Arc, Weak},
};

use anyhow::{Result, anyhow};
use encase::ShaderType;
use imgui::{DrawData, DrawIdx, FontAtlasFlags, TextureId, internal::RawWrapper};
use itertools::Itertools;
use zerocopy::IntoBytes;

/// The renderer structure.
pub struct Renderer {
    device: wgpu::Device,

    surface_config: wgpu::SurfaceConfiguration,

    /// An integer identifying the last frame to be rendered.
    frame_id: u64,

    /// Our graphics pipeline, specifying the settings and shaders to use
    /// to transform vertex attributes into pixels on the screen.
    pipeline: wgpu::RenderPipeline,
    queue: wgpu::Queue,
    staging_belt: wgpu::util::StagingBelt,

    /// Uniform buffer to hold transform state
    uniform_buffer: wgpu::Buffer,
    uniform_bind_group: wgpu::BindGroup,

    vertex_buffer: Option<wgpu::Buffer>,
    index_buffer: Option<wgpu::Buffer>,

    /// The textures that have been created.
    textures: HashMap<TextureId, Weak<Texture>>,
    /// The next texture ID to be assigned.
    next_texture_id: usize,
    _font_texture: Arc<Texture>,
    texture_bind_group_layout: wgpu::BindGroupLayout,
    sampler: wgpu::Sampler,
}

#[derive(Debug)]
pub struct Target {
    /// The surface to render to.
    surface: wgpu::Surface<'static>,
    /// True if the surface is properly configured.
    valid: bool,
}

pub struct Texture {
    pub id: TextureId,
    bind_group: wgpu::BindGroup,
}

pub struct Frame<'a> {
    pub renderer: &'a mut Renderer,
    pub inflight: Option<wgpu::SubmissionIndex>,
}

mod shaders {
    use encase::ShaderType;
    use zerocopy::{Immutable, IntoBytes};

    #[derive(IntoBytes, Immutable, Clone, Copy, Debug)]
    #[repr(C)]
    pub struct DrawVert {
        pub pos: [f32; 2],
        pub uv: [f32; 2],
        pub col: [u8; 4],
    }

    impl DrawVert {
        pub const LAYOUT: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Self>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &wgpu::vertex_attr_array![
                0 => Float32x2,
                1 => Float32x2,
                2 => Uint32
            ],
        };
    }

    #[derive(ShaderType, Clone, Copy, Debug)]
    pub struct Transform {
        pub scale: glam::Vec2,
        pub translate: glam::Vec2,
    }

    pub const SHADERS: &str = r"
        struct VertexOutput {
            @builtin(position) pos: vec4<f32>,
            @location(0) uv: vec2<f32>,
            @location(1) col: vec4<f32>,
        }

        struct Transform {
            scale: vec2<f32>, 
            translate: vec2<f32>, 
        }
        @group(0) @binding(0)
        var<uniform> transform: Transform;

        @vertex
        fn vertex(
            @location(0) pos: vec2<f32>,
            @location(1) uv: vec2<f32>,
            @location(2) col: u32
        ) -> VertexOutput {
            var result: VertexOutput;
            result.pos = vec4<f32>(pos * transform.scale + transform.translate, 0, 1);
            result.uv = uv;
            result.col = vec4<f32>(
                vec4<u32>(col & 0xff, (col >> 8) & 0xff, (col >> 16) & 0xff, col >> 24)
            ) / 0xff;
            return result;
        }

        @group(1) @binding(0)
        var texture: texture_2d<f32>;
        @group(1) @binding(1)
        var sample: sampler;

        @fragment
        fn fragment(vertex: VertexOutput) -> @location(0) vec4<f32> {
            return vertex.col * textureSample(texture, sample, vertex.uv);
        }
    ";
}

impl Renderer {
    const FONT_TEXTURE_ID: TextureId = TextureId::new(0);

    pub fn device(&self) -> &wgpu::Device {
        &self.device
    }
    pub fn queue(&self) -> &wgpu::Queue {
        &self.queue
    }

    pub fn begin(&mut self) -> Frame<'_> {
        Frame {
            inflight: None,
            renderer: self,
        }
    }

    fn _render(
        &mut self,
        target: &mut Target,
        draw_data: &DrawData,
    ) -> Result<wgpu::SubmissionIndex> {
        target.configure(
            &self.device,
            &self.surface_config,
            (draw_data.display_size[0] * draw_data.framebuffer_scale[0]) as u32,
            (draw_data.display_size[1] * draw_data.framebuffer_scale[1]) as u32,
        );

        // Determine the needed vertex and index buffer sizes
        let vertex_buffer_size = (draw_data
            .draw_lists()
            .map(|list| std::mem::size_of_val(list.vtx_buffer()))
            .sum::<usize>()
            + 1) // + 1 so we don't create empty buffer slices
        .next_multiple_of(wgpu::COPY_BUFFER_ALIGNMENT as usize);
        let index_buffer_size = (draw_data
            .draw_lists()
            .map(|list| std::mem::size_of_val(list.idx_buffer()))
            .sum::<usize>()
            + 1)
        .next_multiple_of(wgpu::COPY_BUFFER_ALIGNMENT as usize);

        // Allocate vertex and index buffers if necessary
        let vertex_buffer = if let Some(b) = self.vertex_buffer.as_mut()
            && b.size() >= vertex_buffer_size as u64
        {
            b
        } else {
            self.vertex_buffer
                .insert(self.device.create_buffer(&wgpu::wgt::BufferDescriptor {
                    label: None,
                    size: vertex_buffer_size as u64,
                    usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                    mapped_at_creation: false,
                }))
        };
        let index_buffer = if let Some(b) = self.index_buffer.as_mut()
            && b.size() >= index_buffer_size as u64
        {
            b
        } else {
            self.index_buffer
                .insert(self.device.create_buffer(&wgpu::wgt::BufferDescriptor {
                    label: None,
                    size: index_buffer_size as u64,
                    usage: wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
                    mapped_at_creation: false,
                }))
        };

        let framebuffer = target.surface.get_current_texture()?;

        // Start a command encoder.
        let mut command_encoder = self.device.create_command_encoder(&Default::default());

        {
            let mut vertex_staging_buffer = self.staging_belt.write_buffer(
                &mut command_encoder,
                vertex_buffer,
                0,
                NonZeroU64::new(vertex_buffer_size as u64).unwrap(),
            );
            let mut vertex_slice = vertex_buffer.slice(..);
            let mut vertex_dst = &mut *vertex_staging_buffer;

            let mut index_staging_buffer = self.staging_belt.write_buffer(
                &mut command_encoder,
                index_buffer,
                0,
                NonZeroU64::new(index_buffer_size as u64).unwrap(),
            );
            let mut index_slice = index_buffer.slice(..);
            let mut index_dst = &mut *index_staging_buffer;

            // Upload offset/scale uniforms.
            // Map (x, y) to (-1, -1) and (x+w, y+h) to (1,  1)
            // (0, 0) => (-1 - x*scale, -1 - y*scale)
            // 1 unit in the X direction goes to 2/w units
            let x = draw_data.display_pos[0];
            let y = draw_data.display_pos[1];
            let w = draw_data.display_size[0];
            let h = draw_data.display_size[1];
            let scale = glam::vec2(2. / w, -2. / h);
            let translate = glam::vec2(-1. - x * scale[0], 1. - y * scale[1]);
            let transform = shaders::Transform { scale, translate };
            encase::UniformBuffer::new(&mut *self.staging_belt.write_buffer(
                &mut command_encoder,
                &self.uniform_buffer,
                0,
                transform.size(),
            ))
            .write(&transform)?;

            let mut render_pass = command_encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &framebuffer
                        .texture
                        .create_view(&wgpu::TextureViewDescriptor {
                            usage: Some(self.surface_config.usage),
                            ..Default::default()
                        }),
                    depth_slice: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                    resolve_target: None,
                })],
                ..Default::default()
            });

            // Configure our viewport dimensions.
            let dimensions = framebuffer.texture.size();
            let dimensions = [dimensions.width as f32, dimensions.height as f32];
            render_pass.set_viewport(0., 0., dimensions[0], dimensions[1], 0.0, 1.0);
            render_pass.set_bind_group(0, &self.uniform_bind_group, &[]);

            // Use our graphics pipeline.
            render_pass.set_pipeline(&self.pipeline);

            // Draw our geometry.
            for list in draw_data.draw_lists() {
                // Copy data to vertex and index buffers.
                let vertex_data: &[shaders::DrawVert] = unsafe { list.transmute_vtx_buffer() };
                let index_data: &[DrawIdx] = list.idx_buffer();
                if index_data.is_empty() {
                    continue;
                }

                vertex_dst[..vertex_data.as_bytes().len()].copy_from_slice(vertex_data.as_bytes());
                vertex_dst = &mut vertex_dst[vertex_data.as_bytes().len()..];

                render_pass.set_vertex_buffer(
                    0,
                    vertex_slice.slice(..vertex_data.as_bytes().len() as u64),
                );
                vertex_slice = vertex_slice.slice(vertex_data.as_bytes().len() as u64..);

                index_dst[..index_data.as_bytes().len()].copy_from_slice(index_data.as_bytes());
                index_dst = &mut index_dst[index_data.as_bytes().len()..];
                render_pass.set_index_buffer(
                    index_slice.slice(..index_data.as_bytes().len() as u64),
                    wgpu::IndexFormat::Uint16,
                );
                index_slice = index_slice.slice(index_data.as_bytes().len() as u64..);
                const {
                    assert!(std::mem::size_of::<DrawIdx>() == 2);
                }

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

                            render_pass.set_bind_group(1, &texture.bind_group, &[]);

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

                            render_pass.set_scissor_rect(
                                clip_min[0],
                                clip_min[1],
                                clip_max[0] - clip_min[0],
                                clip_max[1] - clip_min[1],
                            );

                            // Draw the thing.
                            render_pass.draw_indexed(
                                idx_offset as u32..(idx_offset + count) as u32,
                                vtx_offset as i32,
                                0..1,
                            );
                        }
                        imgui::DrawCmd::ResetRenderState => {}
                        imgui::DrawCmd::RawCallback { callback, raw_cmd } => unsafe {
                            callback(list.raw(), raw_cmd);
                        },
                    }
                }
            }
        }
        let command_buffer = command_encoder.finish();

        self.staging_belt.finish();

        // we're done rendering!
        self.frame_id += 1;

        // Submit the command buffer for execution.
        let inflight = self.queue.submit([command_buffer]);

        framebuffer.present();

        // Clean up any old textures.
        self.textures.retain(|_, t| t.strong_count() != 0);

        self.staging_belt.recall();

        Ok(inflight)
    }

    /// Initializes all rendering resources.
    pub fn new(
        instance: &wgpu::Instance,
        surface: &wgpu::Surface,
        imgui: &mut imgui::Context,
    ) -> Result<Self> {
        let runtime = tokio::runtime::Builder::new_current_thread().build()?;
        let adapter = runtime.block_on(instance.request_adapter(&Default::default()))?;
        let (device, queue) = runtime.block_on(adapter.request_device(&Default::default()))?;

        // Choose a surface format.
        let surface_caps = surface.get_capabilities(&adapter);
        let surface_format = *surface_caps
            .formats
            .iter()
            .find_or_first(|f| !f.is_srgb())
            .unwrap();
        let surface_config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: 0,
            height: 0,
            present_mode: wgpu::PresentMode::AutoVsync,
            desired_maximum_frame_latency: 1,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
        };

        // Create an image buffer in GPU memory, and upload it to the GPU.
        imgui.fonts().flags.insert(FontAtlasFlags::NO_BAKED_LINES);
        let font_image = imgui.fonts().build_rgba32_texture();
        let size = wgpu::Extent3d {
            width: font_image.width,
            height: font_image.height,
            depth_or_array_layers: 1,
        };
        let font_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("font_texture"),
            size,
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });
        queue.write_texture(
            wgpu::TexelCopyTextureInfo {
                texture: &font_texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            font_image.data,
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(font_image.width * 4),
                rows_per_image: Some(font_image.height),
            },
            size,
        );

        // Create a sampler that describes how to access our image.
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        // Create a bind group binding our texture & sampler to the graphics pipeline.
        let texture_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: false },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::NonFiltering),
                        count: None,
                    },
                ],
            });

        let uniform_buffer = device.create_buffer(&wgpu::wgt::BufferDescriptor {
            label: None,
            size: std::mem::size_of::<shaders::Transform>() as u64,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        let uniform_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: NonZeroU64::new(
                            std::mem::size_of::<shaders::Transform>() as u64,
                        ),
                    },
                    count: None,
                }],
            });
        let uniform_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &uniform_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                    buffer: &uniform_buffer,
                    offset: 0,
                    size: None,
                }),
            }],
        });

        let shader_module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: None,
            source: wgpu::ShaderSource::Wgsl(shaders::SHADERS.into()),
        });
        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&uniform_bind_group_layout, &texture_bind_group_layout],
            immediate_size: 0,
        });
        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: None,
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader_module,
                entry_point: Some("vertex"),
                compilation_options: Default::default(),
                buffers: &[shaders::DrawVert::LAYOUT],
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader_module,
                entry_point: Some("fragment"),
                compilation_options: Default::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format: surface_config.format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            primitive: Default::default(),
            depth_stencil: None,
            multisample: Default::default(),
            multiview_mask: None,
            cache: None,
        });

        let font_texture = Self::_create_texture(
            Self::FONT_TEXTURE_ID,
            &font_texture,
            &device,
            &texture_bind_group_layout,
            &sampler,
        )?;
        let textures = HashMap::from_iter([(Self::FONT_TEXTURE_ID, Arc::downgrade(&font_texture))]);

        let staging_belt = wgpu::util::StagingBelt::new(device.clone(), 32768);
        let result = Renderer {
            frame_id: 0,

            device,
            surface_config,
            pipeline,
            queue,
            staging_belt,

            uniform_buffer,
            uniform_bind_group,

            vertex_buffer: None,
            index_buffer: None,

            _font_texture: font_texture,
            textures,
            next_texture_id: Self::FONT_TEXTURE_ID.id() + 1,
            sampler,
            texture_bind_group_layout,
        };

        Ok(result)
    }

    pub fn create_texture(&mut self, texture: &wgpu::Texture) -> Result<Arc<Texture>> {
        let id = TextureId::new(self.next_texture_id);

        let texture = Self::_create_texture(
            id,
            texture,
            &self.device,
            &self.texture_bind_group_layout,
            &self.sampler,
        )?;

        self.textures.insert(id, Arc::downgrade(&texture));
        self.next_texture_id += 1;
        Ok(texture)
    }

    fn _create_texture(
        id: TextureId,
        texture: &wgpu::Texture,
        device: &wgpu::Device,
        layout: &wgpu::BindGroupLayout,
        sampler: &wgpu::Sampler,
    ) -> Result<Arc<Texture>> {
        Ok(Arc::new(Texture {
            id,
            bind_group: device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: None,
                layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(
                            &texture.create_view(&Default::default()),
                        ),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(sampler),
                    },
                ],
            }),
        }))
    }
}

impl Frame<'_> {
    pub fn render(&mut self, target: &mut Target, draw_data: &DrawData) -> Result<()> {
        self.inflight = Some(self.renderer._render(target, draw_data)?);
        Ok(())
    }

    pub fn wait(mut self) -> Result<()> {
        if let Some(inflight) = std::mem::take(&mut self.inflight) {
            self.renderer.device.poll(wgpu::PollType::Wait {
                submission_index: Some(inflight),
                timeout: None,
            })?;
        }

        Ok(())
    }
}

impl Target {
    /// Creates a new render target for the given surface.
    pub fn new(surface: wgpu::Surface<'static>) -> Self {
        Self {
            surface,
            valid: false,
        }
    }
    pub fn invalidate(&mut self) {
        self.valid = false;
    }

    fn configure(
        &mut self,
        device: &wgpu::Device,
        config: &wgpu::SurfaceConfiguration,
        width: u32,
        height: u32,
    ) {
        if self.valid {
            return;
        }

        self.surface.configure(
            device,
            &wgpu::SurfaceConfiguration {
                width,
                height,
                ..config.clone()
            },
        );
        self.valid = true;
    }
}
