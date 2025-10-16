#![allow(clippy::mut_from_ref)]
use std::{
    mem::{ManuallyDrop, MaybeUninit},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Weak,
    },
};

use anyhow::Context;
use cpal::traits::{DeviceTrait, HostTrait};
use rubato::Resampler;

use crate::core;

/// The interface from the emulator to the audio system: you write audio samples here; we'll
/// resample, buffer, and play them.
pub struct AudioWriter {
    /// The ring buffer storing samples to be played back.
    /// This buffer stores samples formatted for the host system: a sequence of frames, at the host
    /// sample rate, with the host's preferred number of channels per frame.
    buffer: Arc<RingBuffer<f32>>,

    /// Resamples audio from the libretro guest's sample rate to the host sample rate.
    resampler: rubato::SincFixedOut<f32>,

    /// A buffer of samples waiting to be resampled.
    /// The resampler accepts audio in chunks; once we have enough samples in the buffer, we can
    /// send them to the resampler.
    resampler_input_buffer: [Vec<f32>; 2],

    /// A scratch buffer for the resampler to store its output.
    resampler_output_buffer: [Vec<f32>; 2],

    /// The number of output channels on the host device.
    output_channels: usize,

    /// A handle to the cpal audio stream. Once this is dropped, the stream is stopped.
    _stream: cpal::Stream,
}

impl AudioWriter {
    /// Creates an AudioWriter given the sample rate of the libretro guest.
    pub fn new(input_sample_rate: usize) -> anyhow::Result<AudioWriter> {
        // Get the audio device.
        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .context("No output device available")?;

        let config = device.default_output_config()?;
        let output_sample_rate = config.sample_rate().0 as usize;
        let output_channels = config.channels() as usize;

        // 3 frames at 60fps seems like a reasonable buffer size
        let bufsize = (output_sample_rate / 20).next_power_of_two();

        // Default to a fourth of that (~one frame) for the system's audio buffer size
        // That way we can be sure we always have enough samples for the system,
        // and room for more samples from the emulator
        let device_bufsize = bufsize / 4;
        let device_bufsize = match config.buffer_size() {
            cpal::SupportedBufferSize::Range { min, max } => {
                // But if that's not compatible, clamp it to the system's supported range
                device_bufsize.clamp(*min as usize, *max as usize)
            }
            cpal::SupportedBufferSize::Unknown => device_bufsize,
        };

        let buffer = Arc::new(RingBuffer::new(bufsize * output_channels));

        let mut config = config.config();
        config.buffer_size = cpal::BufferSize::Fixed(device_bufsize as u32);
        let stream = device.build_output_stream(
            &config,
            Self::callback(Arc::downgrade(&buffer), output_sample_rate, output_channels),
            |e| println!("Audio playback error: {e:?}"),
            None,
        )?;

        // Create a resampler to convert from the guest sample rate to the host sample rate.
        let resampler = rubato::SincFixedOut::new(
            output_sample_rate as f64 / input_sample_rate as f64,
            1.,
            // Use recommended paramters from rubato documentation:
            // https://docs.rs/rubato/latest/rubato/struct.SincInterpolationParameters.html
            rubato::SincInterpolationParameters {
                sinc_len: 256,
                f_cutoff: 0.95,
                oversampling_factor: 128,
                interpolation: rubato::SincInterpolationType::Quadratic,
                window: rubato::WindowFunction::Blackman,
            },
            device_bufsize / 2,
            2,
        )?;

        Ok(AudioWriter {
            buffer,
            resampler,
            resampler_input_buffer: [Vec::new(), Vec::new()],
            resampler_output_buffer: [Vec::new(), Vec::new()],
            output_channels,
            _stream: stream,
        })
    }

    /// Writes samples to the audio device.
    pub fn write(&mut self, mut samples: &[core::AudioFrame]) {
        unsafe {
            // Get the available buffer space.
            let write_buf = self.buffer.write_buffer();
            let mut write_buf = write_buf
                .0
                .chunks_exact_mut(self.output_channels)
                .chain(write_buf.1.chunks_exact_mut(self.output_channels))
                .peekable();
            let mut samples_written = 0;

            // As long as we have more samples to process and we have more space in our buffer...
            while !samples.is_empty() && write_buf.peek().is_some() {
                // Put samples into the resampler input buffer.
                while self.resampler_input_buffer[0].len() < self.resampler.input_frames_next()
                    && !samples.is_empty()
                {
                    self.resampler_input_buffer[0]
                        .push((samples[0].l as f32) / (i16::MAX as f32 + 1.));
                    self.resampler_input_buffer[1]
                        .push((samples[0].r as f32) / (i16::MAX as f32 + 1.));
                    samples = &samples[1..];
                }

                // If it's full, resample a batch of samples.
                if self.resampler_input_buffer[0].len() == self.resampler.input_frames_next() {
                    let output_len = self.resampler.output_frames_next();
                    self.resampler_output_buffer[0].resize(output_len, 0.);
                    self.resampler_output_buffer[1].resize(output_len, 0.);
                    self.resampler
                        .process_into_buffer(
                            &self.resampler_input_buffer,
                            &mut self.resampler_output_buffer,
                            None,
                        )
                        .unwrap();
                    self.resampler_input_buffer[0].clear();
                    self.resampler_input_buffer[1].clear();
                }

                // Write samples from the resampler buffer to the playback buffer.
                let interleaved_samples = self.resampler_output_buffer[0]
                    .iter()
                    .zip(self.resampler_output_buffer[1].iter());
                for ((l, r), out) in interleaved_samples.zip(&mut write_buf) {
                    if self.output_channels == 1 {
                        // If the output is mono, downmix the samples.
                        out[0] = MaybeUninit::new((l + r) / 2.);
                    } else {
                        out[0] = MaybeUninit::new(*l);
                        out[1] = MaybeUninit::new(*r);
                        // If the output has more than 2 channels, just use the first 2.
                        out[2..].fill(MaybeUninit::new(0.));
                    }
                    samples_written += self.output_channels;
                }
                self.resampler_output_buffer[0].clear();
                self.resampler_output_buffer[1].clear();
            }

            self.buffer.commit_write(samples_written);
        }
    }

    // Generates a callback function that is invoked when the system requests audio samples.
    fn callback(
        buffer: Weak<RingBuffer<f32>>,
        output_sample_rate: usize,
        output_channels: usize,
    ) -> impl FnMut(&mut [f32], &cpal::OutputCallbackInfo) {
        // The emulator will not always generate samples: it might lag behind sometimes, and it
        // doesn't generate samples at all when paused. This causes audible pops whenever playback
        // stops and starts as the output level jumps between 0 and whatever the emulator's output
        // level is. To mitigate that, we apply gradually ramp up the output level when playback starts
        // and ramp-down when it stops.

        // If true, we aren't currently playing anything and should apply a ramp-up when we start.
        let mut muted = true;
        move |out_buf, _info| {
            // The number of samples we've written to the output stream.
            let mut samples_written = 0;
            // Does the ring buffer still exist? (It might not if the audio writer was destroyed.)
            if let Some(buffer) = buffer.upgrade() {
                unsafe {
                    let read_buffer = buffer.read_buffer();
                    for slice in [read_buffer.0, read_buffer.1] {
                        // Copy some samples to the output stream.
                        let samples_to_write = (out_buf.len() - samples_written).min(slice.len());
                        out_buf[samples_written..][..samples_to_write].copy_from_slice(
                            std::mem::transmute::<&[std::mem::ManuallyDrop<f32>], &[f32]>(
                                &slice[0..samples_to_write],
                            ),
                        );
                        samples_written += samples_to_write;
                    }

                    let ramp_len = output_sample_rate / 1000;
                    if muted && samples_written != 0 {
                        // We were muted, but now we're playing some audio. Apply a ramp-up so it
                        // doesn't pop.
                        let ramp_region = &mut out_buf[0..(ramp_len * 2).min(samples_written)];
                        for (frame, ramp_index) in ramp_region
                            .chunks_exact_mut(output_channels)
                            .zip(0..ramp_len)
                        {
                            let ramp = ramp_index as f32 / ramp_len as f32;
                            frame[0] *= ramp;
                            if let Some(s) = frame.get_mut(1) {
                                *s *= ramp;
                            }
                        }
                    }
                    if samples_written == out_buf.len() {
                        muted = false;
                    } else {
                        // There's not enough samples to fill the buffer, so apply a ramp-down and
                        // set the muted flag.
                        muted = true;
                        let ramp_region = &mut out_buf
                            [samples_written.saturating_sub(ramp_len * 2)..samples_written];
                        for (frame, ramp_index) in ramp_region
                            .chunks_exact_mut(output_channels)
                            .zip(0..ramp_len)
                        {
                            let ramp = (ramp_len - ramp_index) as f32 / ramp_len as f32;
                            frame[0] *= ramp;
                            if let Some(s) = frame.get_mut(1) {
                                *s *= ramp;
                            }
                        }
                    }
                    buffer.commit_read(samples_written);
                }
            }
            out_buf[samples_written..].fill_with(Default::default);
        }
    }
}

/// A lock-free ring buffer for queueing audio samples.
struct RingBuffer<T> {
    /// A pointer to the start of the buffer, of length 'capacity'.
    data: *mut T,
    /// The size of the buffer in elements.
    capacity: usize,
    /// The index of the next value to be read from the buffer, mod 2*capacity.
    reader: AtomicUsize,
    /// The index of the next value to be written to the buffer, mod 2*capacity.
    /// The mod 2*capacity allows distinguishing an empty buffer from a full buffer.
    writer: AtomicUsize,
}

unsafe impl<T> Sync for RingBuffer<T> {}
unsafe impl<T: Send> Send for RingBuffer<T> {}

impl<T> RingBuffer<T> {
    /// Creates a new ring buffer.
    fn new(capacity: usize) -> RingBuffer<T> {
        assert_ne!(capacity, 0);
        unsafe {
            RingBuffer {
                data: std::alloc::alloc(std::alloc::Layout::array::<T>(capacity).unwrap())
                    as *mut T,
                capacity,
                reader: AtomicUsize::new(0),
                writer: AtomicUsize::new(0),
            }
        }
    }
}

impl<T> RingBuffer<T> {
    /// Returns the space available for writing. Since this is a ring buffer, the space is returned
    /// as up to two contiguous slices.
    /// Safety: The caller must ensure only one writer accesses the buffer at a time, including
    /// from separate threads. The preexisting contents of the returned slices are undefined.
    unsafe fn write_buffer(&self) -> (&mut [MaybeUninit<T>], &mut [MaybeUninit<T>]) {
        // Load the writer position. Relaxed ordering is OK since we are guaranteed to be
        // ordered after any previous writer.
        let writer = self.writer.load(Ordering::Relaxed);

        // Load the reader position, with acquire ordering. This ensures all writes to the buffer
        // are ordered after the completion of the previous read operation.
        let reader = self.reader.load(Ordering::Acquire);

        if writer.abs_diff(reader) == self.capacity {
            // The buffer is full.
            return (&mut [], &mut []);
        }

        // Now that we've established the buffer has space, wrap to within the range.
        let writer = writer % self.capacity;
        let reader = reader % self.capacity;

        if writer < reader {
            (
                // Just one slice, from the writer position to the reader position.
                std::slice::from_raw_parts_mut(
                    self.data.add(writer) as *mut MaybeUninit<T>,
                    reader - writer,
                ),
                &mut [],
            )
        } else {
            (
                // Write from the writer position to the end of the buffer...
                std::slice::from_raw_parts_mut(
                    self.data.add(writer) as *mut MaybeUninit<T>,
                    self.capacity - writer,
                ),
                // and from the start of the buffer to the reader position.
                std::slice::from_raw_parts_mut(self.data as *mut MaybeUninit<T>, reader),
            )
        }
    }

    /// Advances the writer position to commit elements to the buffer.
    /// Safety: The caller must ensure only one writer accesses the buffer at a time, including
    /// from separate threads. The caller must have initialized the next `len` elements of the
    /// buffer prior to calling this function. `len` must be less than or equal to the available
    /// capacity, as determined by `write_buffer`.
    unsafe fn commit_write(&self, len: usize) {
        // Load the old writer position. Relaxed ordering is OK since we are guaranteed to be
        // ordered after any previous writer.
        let writer = self.writer.load(Ordering::Relaxed);

        // Commit the new writer position, with release ordering to ensure all writes to the buffer
        // are ordered before the writer position is aquired by a read operation.
        self.writer
            .store((writer + len) % (2 * self.capacity), Ordering::Release);
    }

    /// Returns the data available for reading. Since this is a ring buffer, the space is returned
    /// as up to two contiguous slices.
    /// Safety: The caller must ensure only one reader accesses the buffer at a time, including
    /// from separate threads. If the elements need to be dropped, the caller must do so manually.
    unsafe fn read_buffer(&self) -> (&mut [ManuallyDrop<T>], &mut [ManuallyDrop<T>]) {
        // Load the reader position. Relaxed ordering is OK since we are guaranteed to be
        // ordered after any previous reader.
        let writer = self.writer.load(Ordering::Relaxed);

        // Load the writer position, with acquire ordering. This ensures all reads from the buffer
        // are ordered after the completion of the previous write operation.
        let reader = self.reader.load(Ordering::Acquire);

        if writer == reader {
            // The buffer is empty.
            return (&mut [], &mut []);
        }

        // Now that we've established the buffer has data, wrap to within the range.
        let writer = writer % self.capacity;
        let reader = reader % self.capacity;

        if reader < writer {
            (
                // Just one slice, from the reader position to the writer position.
                std::slice::from_raw_parts_mut(
                    self.data.add(reader) as *mut ManuallyDrop<T>,
                    writer - reader,
                ),
                &mut [],
            )
        } else {
            (
                // Read from the reader position to the end of the buffer...
                std::slice::from_raw_parts_mut(
                    self.data.add(reader) as *mut ManuallyDrop<T>,
                    self.capacity - reader,
                ),
                // and from the start of the buffer to the writer position.
                std::slice::from_raw_parts_mut(self.data as *mut ManuallyDrop<T>, writer),
            )
        }
    }

    /// Advances the reader position to commit elements to the buffer.
    /// Safety: The caller must ensure only one reader accesses the buffer at a time, including
    /// from separate threads. The caller must not access the next `len` readable elements after
    /// calling this function. `len` must be less than or equal the number of readable elements.
    unsafe fn commit_read(&self, len: usize) {
        // Load the old reader position. Relaxed ordering is OK since we are guaranteed to be
        // ordered after any previous reader.
        let reader = self.reader.load(Ordering::Relaxed);

        // Commit the new reader position, with release ordering to ensure all reads from the buffer
        // are ordered before the reader position is aquired by a write operation.
        self.reader
            .store((reader + len) % (2 * self.capacity), Ordering::Release);
    }
}
