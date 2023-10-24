use std::io::{Read, Write};

use flate2::{
    bufread::{DeflateDecoder, DeflateEncoder},
    Compression,
};

/// A reference to an uncompressed savestate living somewhere in memory.
#[derive(Clone, Copy)]
pub struct SavestateBuffer<'a> {
    pub(super) state: &'a [u8],
    pub(super) core_id: usize,
}

impl<'a> SavestateBuffer<'a> {
    pub(super) fn new(state: &'a [u8], core_id: usize) -> SavestateBuffer<'a> {
        SavestateBuffer { state, core_id }
    }

    pub fn compress(&self) -> Savestate {
        let mut buf = Vec::new();
        DeflateEncoder::new(self.state, Compression::fast())
            .read_to_end(&mut buf)
            .unwrap();
        buf.shrink_to_fit();

        Savestate {
            state: buf.into_boxed_slice(),
            core_id: self.core_id,
        }
    }
}

/// A reference to a compressed savestate living somewhere in memory.
#[derive(Clone, Copy)]
pub enum SavestateRef<'a> {
    Full(&'a Savestate),
    Delta(&'a DeltaSavestate, &'a Savestate),
}

impl<'a> SavestateRef<'a> {
    pub(super) fn core_id(&self) -> usize {
        match self {
            SavestateRef::Full(s) => s.core_id,
            SavestateRef::Delta(s, _) => s.core_id,
        }
    }
    pub(super) fn decompress(&self, buf: &mut [u8]) {
        match self {
            SavestateRef::Full(state) => state.decompress(buf),
            SavestateRef::Delta(state, parent) => state.decompress(parent, buf),
        }
    }
}

/// An owned, compressed savestate.
pub struct Savestate {
    state: Box<[u8]>,
    pub(super) core_id: usize,
}

impl Savestate {
    pub fn size(&self) -> usize {
        self.state.len()
    }

    pub(super) fn decompress(&self, buf: &mut [u8]) {
        let mut decoder = DeflateDecoder::new(&*self.state);
        decoder.read_exact(buf).unwrap();
        assert!(decoder.read(&mut [0]).unwrap() == 0, "expected EOF");
    }

    pub fn to_delta(&self, parent: &Savestate) -> DeltaSavestate {
        use flate2::write::DeflateEncoder;
        const BUFSIZE: usize = 16834;

        let mut self_decoder = DeflateDecoder::new(&*self.state);
        let mut parent_decoder = DeflateDecoder::new(&*parent.state);

        let mut dst_buf = Vec::new();
        let mut encoder = DeflateEncoder::new(&mut dst_buf, Compression::fast());

        let mut a = [0u8; BUFSIZE];
        let mut b = [0u8; BUFSIZE];

        loop {
            let len = self_decoder.read(&mut a).unwrap();
            if len == 0 {
                assert!(parent_decoder.read(&mut [0]).unwrap() == 0);
                break;
            }

            parent_decoder.read_exact(&mut b[..len]).unwrap();

            a.iter_mut().take(len).zip(b).for_each(|(a, b)| *a ^= b);
            encoder.write_all(&a[..len]).unwrap();
        }

        encoder.finish().unwrap();

        dst_buf.shrink_to_fit();
        DeltaSavestate {
            state: dst_buf.into_boxed_slice(),
            parent_addr: &*parent.state,
            core_id: self.core_id,
        }
    }
}

pub struct DeltaSavestate {
    state: Box<[u8]>,
    parent_addr: *const [u8],
    pub(super) core_id: usize,
}
unsafe impl Send for DeltaSavestate {}

impl DeltaSavestate {
    pub fn size(&self) -> usize {
        self.state.len()
    }

    pub(super) fn decompress(&self, parent: &Savestate, dst_buf: &mut [u8]) {
        const BUFSIZE: usize = 16834;

        assert!(self.parent_addr == &*parent.state as *const [u8]);
        assert!(self.core_id == parent.core_id);

        parent.decompress(dst_buf);

        let mut i = &mut dst_buf[..];
        let mut delta_buf = [0u8; BUFSIZE];
        let mut decoder = DeflateDecoder::new(&*self.state);

        loop {
            let len = decoder.read(&mut delta_buf).unwrap();
            if len == 0 {
                assert_eq!(i.len(), 0);
                break;
            }

            let j = &mut delta_buf[..len];
            i.iter_mut().zip(j.iter()).for_each(|(i, j)| *i ^= *j);
            i = &mut i[len..];
        }
    }

    pub fn reparent(&mut self, old_parent: &Savestate, new_parent: &Savestate) {
        use flate2::write::DeflateEncoder;
        const BUFSIZE: usize = 16834;

        assert!(self.core_id == old_parent.core_id);
        assert!(self.core_id == new_parent.core_id);
        assert!(self.parent_addr == &*old_parent.state as *const [u8]);

        let mut self_decoder = DeflateDecoder::new(&*self.state);
        let mut old_decoder = DeflateDecoder::new(&*old_parent.state);
        let mut new_decoder = DeflateDecoder::new(&*new_parent.state);

        let mut dst_buf = Vec::new();
        let mut encoder = DeflateEncoder::new(&mut dst_buf, Compression::fast());

        let mut a = [0u8; BUFSIZE];
        let mut o = [0u8; BUFSIZE];
        let mut n = [0u8; BUFSIZE];

        loop {
            let len = self_decoder.read(&mut a).unwrap();
            if len == 0 {
                assert!(old_decoder.read(&mut [0]).unwrap() == 0);
                assert!(new_decoder.read(&mut [0]).unwrap() == 0);
                break;
            }

            old_decoder.read_exact(&mut o[..len]).unwrap();
            new_decoder.read_exact(&mut n[..len]).unwrap();

            a.iter_mut()
                .take(len)
                .zip(o)
                .zip(n)
                .for_each(|((a, o), n)| *a ^= o ^ n);
            encoder.write_all(&a[..len]).unwrap();
        }

        encoder.finish().unwrap();

        dst_buf.shrink_to_fit();
        self.state = dst_buf.into_boxed_slice();
        self.parent_addr = &*new_parent.state as *const [u8];
    }
}
