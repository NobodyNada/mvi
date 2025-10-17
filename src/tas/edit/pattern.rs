use std::rc::Rc;

use crate::tas::input::InputPort;

#[derive(Clone, Debug)]
pub struct Pattern {
    pub buf: Rc<PatternBuf>,
    pub offset: usize,
}

#[derive(Clone, Debug)]
pub struct PatternBuf {
    pub data: Vec<u8>,
    pub frame_size: usize,
    pub mask: Option<Vec<bool>>,
    pub autofire: Option<Vec<bool>>,
    pub autohold: Option<Vec<bool>>,
}

impl PatternBuf {
    pub fn len(&self) -> usize {
        self.data.len() / self.frame_size
    }
}

#[allow(dead_code)]
impl Pattern {
    pub fn read(&self, input_ports: &[InputPort], port: u32, index: u32, id: u32) -> i16 {
        let port = port as usize;
        assert!(port < input_ports.len());
        let offset: usize = input_ports[0..port]
            .iter()
            .map(crate::tas::input::InputPort::frame_size)
            .sum();
        let offset = offset + (self.offset * self.buf.frame_size) % self.buf.len();
        let len = input_ports[port].frame_size();

        input_ports[port].read(&self.buf.data[offset..(offset + len)], index, id)
    }

    pub fn write(&mut self, input_ports: &[InputPort], port: u32, index: u32, id: u32, value: i16) {
        let port = port as usize;
        assert!(port < input_ports.len());
        let offset: usize = input_ports[0..port]
            .iter()
            .map(crate::tas::input::InputPort::frame_size)
            .sum();
        let offset = offset + (self.offset * self.buf.frame_size) % self.buf.len();
        let len = input_ports[port].frame_size();

        input_ports[port].write(
            &mut Rc::make_mut(&mut self.buf).data[offset..(offset + len)],
            index,
            id,
            value,
        )
    }

    pub fn apply(&self, input_ports: &[InputPort], dst: &mut [u8]) -> Pattern {
        assert_eq!(dst.len() % self.buf.frame_size, 0);

        let mut offset = self.offset;
        for dst_frame in dst.chunks_exact_mut(self.buf.frame_size) {
            let src_frame = &self.buf.data
                [((offset * self.buf.frame_size) % self.buf.data.len())..][..self.buf.frame_size];
            let autofire_value = offset % (2 * self.buf.len()) < self.buf.len();

            let mut i = 0;
            let mut device_offset = 0;
            for device in input_ports {
                let src = &src_frame[device_offset..(device_offset + device.frame_size())];
                let dst = &mut dst_frame[device_offset..(device_offset + device.frame_size())];
                for index in 0..device.num_controllers() {
                    for id in 0..device.num_inputs(index) {
                        if self.buf.mask.as_ref().map(|mask| mask[i]).unwrap_or(true) {
                            let autofire_value = autofire_value
                                & self.buf.autofire.as_ref().map(|af| af[i]).unwrap_or(false);
                            device.write(
                                dst,
                                index,
                                id,
                                device.read(src, index, id) ^ (autofire_value as i16),
                            );
                        }
                        i += 1;
                    }
                }
                device_offset += device.frame_size();
            }

            offset += 1;
        }

        Pattern {
            buf: self.buf.clone(),
            offset,
        }
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn mask(&self, input_ports: &[InputPort], port: u32, index: u32, id: u32) -> bool {
        self.buf
            .mask
            .as_ref()
            .map(|mask| mask[Self::input_index(input_ports, port, index, id)])
            .unwrap_or(true)
    }

    pub fn autofire(&self, input_ports: &[InputPort], port: u32, index: u32, id: u32) -> bool {
        self.buf
            .autofire
            .as_ref()
            .map(|af| af[Self::input_index(input_ports, port, index, id)])
            .unwrap_or(false)
    }

    pub fn autohold(&self, input_ports: &[InputPort], port: u32, index: u32, id: u32) -> bool {
        self.buf
            .autohold
            .as_ref()
            .map(|ah| ah[Self::input_index(input_ports, port, index, id)])
            .unwrap_or(false)
    }

    pub fn mask_mut(
        &mut self,
        input_ports: &[InputPort],
        port: u32,
        index: u32,
        id: u32,
    ) -> &mut bool {
        let mask = Rc::make_mut(&mut self.buf).mask.get_or_insert_with(|| {
            std::iter::repeat_n(true, Self::input_count(input_ports)).collect()
        });
        &mut mask[Self::input_index(input_ports, port, index, id)]
    }

    pub fn autofire_mut(
        &mut self,
        input_ports: &[InputPort],
        port: u32,
        index: u32,
        id: u32,
    ) -> &mut bool {
        let af = Rc::make_mut(&mut self.buf).autofire.get_or_insert_with(|| {
            std::iter::repeat_n(false, Self::input_count(input_ports)).collect()
        });
        &mut af[Self::input_index(input_ports, port, index, id)]
    }

    pub fn autohold_mut(
        &mut self,
        input_ports: &[InputPort],
        port: u32,
        index: u32,
        id: u32,
    ) -> &mut bool {
        let ah = Rc::make_mut(&mut self.buf).autohold.get_or_insert_with(|| {
            std::iter::repeat_n(false, Self::input_count(input_ports)).collect()
        });
        &mut ah[Self::input_index(input_ports, port, index, id)]
    }

    fn input_index(input_ports: &[InputPort], port: u32, index: u32, id: u32) -> usize {
        input_ports[0..port as usize]
            .iter()
            .map(InputPort::total_inputs)
            .sum::<u32>() as usize
            + (0..index)
                .map(|i| input_ports[port as usize].num_inputs(i))
                .sum::<u32>() as usize
            + id as usize
    }

    fn input_count(input_ports: &[InputPort]) -> usize {
        input_ports.iter().map(InputPort::total_inputs).sum::<u32>() as usize
    }

    /// Cycles the pattern to fill the buffer with `len` frames.
    /// For example, consider the following pattern:
    ///     A     L
    ///      B     R
    /// Calling pattern.expand(8) expands the pattern to 8 frames:
    ///     A     L
    ///      B     R
    ///     A     L
    ///      B     R
    ///     A     L
    ///      B     R
    ///     A     L
    ///      B     R
    /// Note that pattern expansion disables autofire and autohold, at least for now.
    pub fn expand(&mut self, input_ports: &[InputPort], len: u32) {
        let old_frames = self.buf.data.len() / self.buf.frame_size;
        if self.buf.data.len() / self.buf.frame_size < len as usize {
            let mut buf = PatternBuf::clone(&self.buf);
            buf.data.resize(len as usize * buf.frame_size, 0);
            self.apply(input_ports, &mut buf.data[old_frames * buf.frame_size..]);
            self.buf = Rc::new(buf);
        }
    }
}
