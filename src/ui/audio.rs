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

pub struct AudioWriter {
    buffer: Arc<RingBuffer<f32>>,
    resampler: rubato::SincFixedOut<f32>,
    resampler_input_buffer: [Vec<f32>; 2],
    resampler_output_buffer: [Vec<f32>; 2],
    output_channels: usize,
    _stream: cpal::Stream,
}

impl AudioWriter {
    pub fn new(input_sample_rate: usize) -> anyhow::Result<AudioWriter> {
        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .context("No output device available")?;

        let config = device.default_output_config()?;
        let output_sample_rate = config.sample_rate().0 as usize;
        // 3 frames at 60fps seems like a reasonable buffer size
        let bufsize = output_sample_rate / 20;

        let device_bufsize = bufsize / 2;
        let device_bufsize = match config.buffer_size() {
            cpal::SupportedBufferSize::Range { min, max } => {
                device_bufsize.clamp(*min as usize, *max as usize)
            }
            cpal::SupportedBufferSize::Unknown => device_bufsize,
        };
        let output_channels = config.channels() as usize;

        let buffer = Arc::new(RingBuffer::new(bufsize * output_channels));

        let mut config = config.config();
        config.buffer_size = cpal::BufferSize::Fixed(device_bufsize as u32);
        let stream = device.build_output_stream(
            &config,
            Self::callback(Arc::downgrade(&buffer), output_sample_rate),
            |e| println!("Audio playback error: {e:?}"),
            None,
        )?;

        let resampler = rubato::SincFixedOut::new(
            output_sample_rate as f64 / input_sample_rate as f64,
            1.,
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

    pub fn write(&mut self, mut samples: &[core::AudioFrame]) {
        unsafe {
            let write_buf = self.buffer.write_buffer();
            let mut write_buf = write_buf
                .0
                .chunks_exact_mut(self.output_channels)
                .chain(write_buf.1.chunks_exact_mut(self.output_channels))
                .peekable();
            let mut samples_written = 0;

            while !samples.is_empty() && write_buf.peek().is_some() {
                while self.resampler_input_buffer[0].len() < self.resampler.input_frames_next()
                    && !samples.is_empty()
                {
                    self.resampler_input_buffer[0]
                        .push((samples[0].l as f32) / (i16::MAX as f32 + 1.));
                    self.resampler_input_buffer[1]
                        .push((samples[0].r as f32) / (i16::MAX as f32 + 1.));
                    samples = &samples[1..];
                }
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
                        out[0] = MaybeUninit::new((l + r) / 2.);
                    } else {
                        out[0] = MaybeUninit::new(*l);
                        out[1] = MaybeUninit::new(*r);
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

    fn callback(
        buffer: Weak<RingBuffer<f32>>,
        output_sample_rate: usize,
    ) -> impl FnMut(&mut [f32], &cpal::OutputCallbackInfo) {
        let mut muted = true;
        move |out_buf, _info| {
            let mut samples_written = 0;
            if let Some(buffer) = buffer.upgrade() {
                unsafe {
                    let read_buffer = buffer.read_buffer();
                    for slice in [read_buffer.0, read_buffer.1] {
                        let samples_to_write = (out_buf.len() - samples_written).min(slice.len());
                        out_buf[samples_written..][..samples_to_write]
                            .copy_from_slice(std::mem::transmute(&slice[0..samples_to_write]));
                        samples_written += samples_to_write;
                    }

                    let ramp_len = output_sample_rate / 1000;
                    if muted && samples_written != 0 {
                        let ramp_region = &mut out_buf[0..(ramp_len * 2).min(samples_written)];
                        for (frame, ramp_index) in ramp_region.chunks_exact_mut(2).zip(0..ramp_len)
                        {
                            let ramp = ramp_index as f32 / ramp_len as f32;
                            frame[0] *= ramp;
                            frame[1] *= ramp;
                        }
                    }
                    if samples_written == out_buf.len() {
                        muted = false;
                    } else {
                        muted = true;
                        let ramp_region = &mut out_buf
                            [samples_written.saturating_sub(ramp_len * 2)..samples_written];
                        for (frame, ramp_index) in ramp_region.chunks_exact_mut(2).zip(0..ramp_len)
                        {
                            let ramp = (ramp_len - ramp_index) as f32 / ramp_len as f32;
                            frame[0] *= ramp;
                            frame[1] *= ramp;
                        }
                    }
                    buffer.commit_read(samples_written);
                }
            }
            out_buf[samples_written..].fill_with(Default::default);
        }
    }
}

struct RingBuffer<T> {
    data: *mut T,
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
