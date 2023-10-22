use std::sync::{Arc, Mutex};

use anyhow::Result;
use vk::{command_buffer::PrimaryCommandBufferAbstract, sync::GpuFuture};
use vulkano as vk;

use crate::tas::Tas;
mod backend;
mod keybinds;
mod piano_roll;

// TODO: properly handle framerate

#[allow(clippy::collapsible_match, clippy::single_match)]
pub fn run() -> Result<()> {
    let tas = Mutex::new(Tas::new()?);
    let mut framebuffer: Option<Framebuffer> = None;
    let piano_roll = Mutex::new(piano_roll::PianoRoll::new());
    let mut keybinds = keybinds::Keybinds::new();

    let mut modifiers = winit::event::ModifiersState::default();

    backend::run(
        |event, _imgui, _window| {
            let tas = &mut tas.lock().unwrap();
            let piano_roll = &mut piano_roll.lock().unwrap();
            match event {
                winit::event::Event::WindowEvent {
                    window_id: _,
                    event: e,
                } => match e {
                    winit::event::WindowEvent::ReceivedCharacter(c) => {
                        keybinds.input_char(*c, tas, piano_roll)
                    }
                    winit::event::WindowEvent::ModifiersChanged(m) => modifiers = *m,
                    winit::event::WindowEvent::KeyboardInput {
                        input:
                            winit::event::KeyboardInput {
                                state,
                                virtual_keycode: Some(key),
                                ..
                            },
                        ..
                    } => match state {
                        winit::event::ElementState::Pressed => {
                            keybinds.key_down(*key, modifiers, tas, piano_roll)
                        }
                        winit::event::ElementState::Released => {
                            keybinds.key_up(*key, modifiers, tas, piano_roll)
                        }
                    },
                    _ => {}
                },
                _ => {}
            }
        },
        |ui, renderer| {
            let mut tas = tas.lock().unwrap();
            let mut piano_roll = &mut piano_roll.lock().unwrap();

            let av = tas.av_info();
            // Create the framebuffer, if it does not already exist
            let framebuffer = framebuffer.get_or_insert_with(|| {
                let buffer: vk::buffer::Subbuffer<[[u8; 4]]> = vk::buffer::Buffer::new_slice(
                    renderer.context().memory_allocator(),
                    vk::buffer::sys::BufferCreateInfo {
                        usage: vk::buffer::BufferUsage::TRANSFER_SRC,
                        ..Default::default()
                    },
                    vk::memory::allocator::AllocationCreateInfo {
                        usage: vk::memory::allocator::MemoryUsage::Upload,
                        allocate_preference:
                            vk::memory::allocator::MemoryAllocatePreference::AlwaysAllocate,
                        ..Default::default()
                    },
                    (av.geometry.max_width * tas.av_info().geometry.max_height) as u64,
                )
                .unwrap();

                let image = vk::image::StorageImage::with_usage(
                    renderer.context().memory_allocator(),
                    vk::image::ImageDimensions::Dim2d {
                        width: av.geometry.max_width,
                        height: av.geometry.max_height,
                        array_layers: 1,
                    },
                    vk::format::Format::R8G8B8A8_UNORM,
                    vk::image::ImageUsage::TRANSFER_DST | vk::image::ImageUsage::SAMPLED,
                    Default::default(),
                    std::iter::once(renderer.context().graphics_queue().queue_family_index()),
                )
                .unwrap();

                let texture = renderer
                    .create_texture(
                        vk::image::view::ImageView::new(
                            image.clone(),
                            vk::image::view::ImageViewCreateInfo::from_image(&image),
                        )
                        .unwrap(),
                    )
                    .unwrap();

                Framebuffer {
                    buffer,
                    image,
                    texture,
                }
            });

            // Run the emulator
            let frame = tas.run_host_frame();

            // Upload the frame to the GPU
            framebuffer.buffer.write().unwrap()[..frame.buffer.len()]
                .copy_from_slice(&frame.buffer);
            let mut command_buffer = vk::command_buffer::AutoCommandBufferBuilder::primary(
                renderer.command_buffer_allocator(),
                renderer.context().graphics_queue().queue_family_index(),
                vk::command_buffer::CommandBufferUsage::OneTimeSubmit,
            )
            .unwrap();
            command_buffer
                .copy_buffer_to_image(vk::command_buffer::CopyBufferToImageInfo::buffer_image(
                    framebuffer.buffer.clone(),
                    framebuffer.image.clone(),
                ))
                .unwrap();
            command_buffer
                .build()
                .unwrap()
                .execute(renderer.context().graphics_queue().clone())
                .unwrap()
                .then_signal_fence_and_flush()
                .unwrap()
                .wait(None)
                .unwrap();

            // Render the UI
            ui.dockspace_over_main_viewport();

            ui.window("Game View")
                .size([512., 448.], imgui::Condition::FirstUseEver)
                .build(|| {
                    let aspect_ratio = av.geometry.aspect_ratio;
                    let available_size = ui.content_region_avail();
                    let (w, h) = [
                        (available_size[0], available_size[0] / aspect_ratio),
                        (available_size[1] * aspect_ratio, available_size[1]),
                    ]
                    .into_iter()
                    .filter(|(w, h)| w <= &available_size[0] && h <= &available_size[1])
                    .max_by(|(w1, _), (w2, _)| w1.partial_cmp(w2).unwrap())
                    .unwrap();

                    let crop_x = frame.width as f32 / av.geometry.max_width as f32;
                    let crop_y = frame.height as f32 / av.geometry.max_height as f32;
                    imgui::Image::new(framebuffer.texture.id, [w, h])
                        .uv1([crop_x, crop_y])
                        .build(ui)
                });

            piano_roll.draw(ui, &mut tas);
        },
    )
}

struct Framebuffer {
    buffer: vk::buffer::Subbuffer<[[u8; 4]]>,
    image: Arc<vk::image::StorageImage>,
    texture: Arc<backend::render::Texture>,
}
