use std::{
    fmt::Debug,
    io::{Read, Write},
    sync::Arc,
};

use flate2::{
    Compression,
    bufread::{ZlibDecoder, ZlibEncoder},
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
        ZlibEncoder::new(self.state, Compression::fast())
            .read_to_end(&mut buf)
            .unwrap();
        buf.shrink_to_fit();

        Savestate {
            state: buf.into(),
            core_id: self.core_id,
        }
    }
}

/// Either a Savestate, or a DeltaSavestate paired with its parent.
#[derive(Clone)]
pub enum SavestateRef {
    Full(Savestate),
    Delta(DeltaSavestate),
}

impl SavestateRef {
    pub(super) fn core_id(&self) -> usize {
        match self {
            SavestateRef::Full(s) => s.core_id,
            SavestateRef::Delta(s) => s.core_id,
        }
    }
    pub(super) fn decompress(&self, buf: &mut [u8]) {
        match self {
            SavestateRef::Full(state) => state.decompress(buf),
            SavestateRef::Delta(state) => state.decompress(buf),
        }
    }
}

/// A savestate that is compressed, but not with delta compression.
#[derive(Clone)]
pub struct Savestate {
    state: Arc<[u8]>,
    pub(super) core_id: usize,
}

impl Savestate {
    pub fn size(&self) -> usize {
        self.state.len()
    }

    pub(super) fn decompress(&self, buf: &mut [u8]) {
        let mut decoder = ZlibDecoder::new(&*self.state);
        decoder.read_exact(buf).unwrap();
        assert!(decoder.read(&mut [0]).unwrap() == 0, "expected EOF");
    }

    pub fn to_delta(&self, parent: &Savestate) -> DeltaSavestate {
        use flate2::write::ZlibEncoder;
        const BUFSIZE: usize = 16834;

        let mut self_decoder = ZlibDecoder::new(&*self.state);
        let mut parent_decoder = ZlibDecoder::new(&*parent.state);

        let mut dst_buf = Vec::new();
        let mut encoder = ZlibEncoder::new(&mut dst_buf, Compression::fast());

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
            state: dst_buf.into(),
            parent: parent.state.clone(),
            core_id: self.core_id,
        }
    }
}

impl Debug for Savestate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Savestate (compressed size: {}, addr: {:?})",
            self.size(),
            &*self.state as *const [u8]
        )
    }
}

#[derive(Clone)]
pub struct DeltaSavestate {
    state: Arc<[u8]>,
    parent: Arc<[u8]>,
    pub(super) core_id: usize,
}
impl Debug for DeltaSavestate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DeltaSavestate (compressed size: {})", self.size())
    }
}
unsafe impl Send for DeltaSavestate {}

impl DeltaSavestate {
    pub fn size(&self) -> usize {
        self.state.len()
    }

    pub fn parent_is(&self, other: &Savestate) -> bool {
        self.parent == other.state
    }

    pub(super) fn decompress(&self, dst_buf: &mut [u8]) {
        const BUFSIZE: usize = 16834;

        let mut parent_decoder = ZlibDecoder::new(&*self.parent);

        let mut i = &mut dst_buf[..];
        let mut delta_buf = [0u8; BUFSIZE];
        let mut decoder = ZlibDecoder::new(&*self.state);

        loop {
            let len = decoder.read(&mut delta_buf).unwrap();
            if len == 0 {
                assert_eq!(i.len(), 0);
                assert!(parent_decoder.read(&mut [0]).unwrap() == 0);
                break;
            }
            parent_decoder.read_exact(&mut i[..len]).unwrap();

            let j = &mut delta_buf[..len];
            i.iter_mut().zip(j.iter()).for_each(|(i, j)| *i ^= *j);
            i = &mut i[len..];
        }
    }

    pub fn reparent(&mut self, new_parent: &Savestate) {
        use flate2::write::ZlibEncoder;
        const BUFSIZE: usize = 16834;

        assert!(self.core_id == new_parent.core_id);

        let mut self_decoder = ZlibDecoder::new(&*self.state);
        let mut old_decoder = ZlibDecoder::new(&*self.parent);
        let mut new_decoder = ZlibDecoder::new(&*new_parent.state);

        let mut dst_buf = Vec::new();
        let mut encoder = ZlibEncoder::new(&mut dst_buf, Compression::fast());

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
        self.state = dst_buf.into();
        self.parent = new_parent.state.clone();
    }
}
