use std::{ffi::c_void, sync::Arc};

use libretro_ffi::*;
use zerocopy::FromBytes;

use super::{Core, lock};

pub struct Fields {
    pub fields: Vec<Field>,
    pub end: usize,
}

#[derive(Clone)]
pub struct Trace {
    buf: Vec<u8>,
    fields: Arc<Fields>,
}

#[derive(Debug)]
pub struct Field {
    pub name: String,
    pub offset: usize,
    pub size: usize,
    pub field_type: FieldType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldType {
    Register,
    ProgramCounter,
    StackPointer,
    Timing,
}

impl From<retro_trace_field_flags_t> for FieldType {
    #[allow(non_upper_case_globals)]
    fn from(value: retro_trace_field_flags_t) -> Self {
        match value & retro_trace_field_flags_t_RETRO_TRACE_FIELD_TYPE_MASK {
            retro_trace_field_type_t_RETRO_TRACE_FIELD_TYPE_REG => FieldType::Register,
            retro_trace_field_type_t_RETRO_TRACE_FIELD_TYPE_PC => FieldType::ProgramCounter,
            retro_trace_field_type_t_RETRO_TRACE_FIELD_TYPE_SP => FieldType::StackPointer,
            retro_trace_field_type_t_RETRO_TRACE_FIELD_TYPE_TIMING => FieldType::Timing,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct FieldValue<'a> {
    pub field: &'a Field,
    pub data: u64,
}

#[derive(Clone, Copy)]
pub struct Entry<'a> {
    pub data: &'a [u8],
    pub fields: &'a Arc<Fields>,
}

impl Core {
    pub fn begin_trace(&mut self) {
        let mut core = self.lock();
        assert!(
            !core.trace_context.is_null(),
            "begin_trace called on a core that does not support tracing"
        );

        unsafe {
            assert!(
                (*core.trace_context).callback.is_none(),
                "begin_trace called while tracing is already active"
            );
            (*core.trace_context).callback = Some(callback);
        }
        extern "C" fn callback(_ctx: *mut retro_trace_ctx_t, buf: *const c_void, len: usize) {
            unsafe {
                lock()
                    .trace_buffer
                    .extend_from_slice(&*std::ptr::slice_from_raw_parts(buf.cast(), len));
            }
        }
    }

    pub fn end_trace(&mut self) -> Trace {
        let mut core = self.lock();
        unsafe {
            assert!(
                !core.trace_context.is_null(),
                "end_trace called on a core that does not support tracing"
            );
            assert!(
                (*core.trace_context).callback.is_some(),
                "end_trace called with no active trace"
            );
            (*core.trace_context).callback = None;
        }
        Trace {
            buf: std::mem::take(&mut core.trace_buffer),
            fields: self.trace_fields.clone().unwrap(),
        }
    }
}

impl<'a> IntoIterator for &'a Trace {
    type Item = Entry<'a>;
    type IntoIter = TraceIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TraceIter {
            buf: &self.buf,
            fields: &self.fields,
        }
    }
}

pub struct TraceIter<'a> {
    buf: &'a [u8],
    fields: &'a Arc<Fields>,
}

impl<'a> Iterator for TraceIter<'a> {
    type Item = Entry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.buf.is_empty() {
            None
        } else {
            let data = self.buf;
            let (header, _) =
                retro_trace_elem_header_t::read_from_prefix(data).expect("Trace data too short");
            self.buf = &self.buf[header.elem_size..];
            let data = &data[..header.elem_size];
            Some(Entry {
                data,
                fields: self.fields,
            })
        }
    }
}

impl<'a> Entry<'a> {
    pub fn cycle(&self) -> u64 {
        let header_ptr: *const retro_trace_elem_header_t = self.data.as_ptr().cast();
        unsafe { (&raw const (*header_ptr).cycle).read_unaligned() }
    }

    pub fn get(&self, index: usize) -> FieldValue<'a> {
        let field = &self.fields.fields[index];
        self.get_field(field)
    }

    pub fn get_field<'f>(&self, field: &'f Field) -> FieldValue<'f> {
        // FIXME: seen crash here when loading another movie while trace logger is open
        assert!(
            self.fields
                .fields
                .as_ptr_range()
                .contains(&&raw const *field)
        );

        let bytes_in_place = &self.data[field.offset..][..field.size];

        // Zero-extend bytes to u64
        let mut bytes = [0u8; 8];

        #[cfg(target_endian = "little")]
        bytes[0..field.size].copy_from_slice(bytes_in_place);

        #[cfg(target_endian = "big")]
        bytes[8 - field.size..8].copy_from_slice(bytes_in_place);

        FieldValue {
            field,
            data: u64::from_ne_bytes(bytes),
        }
    }

    pub fn instruction_bytes(&self) -> &[u8] {
        let header_ptr: *const retro_trace_elem_header_t = self.data.as_ptr().cast();
        let num_memory_effects =
            unsafe { (&raw const (*header_ptr).num_memory_effects).read_unaligned() };
        let memory_effects_size = num_memory_effects * size_of::<retro_trace_memory_effect_t>();
        &self.data[self.fields.end + memory_effects_size..]
    }
}

impl<'a> IntoIterator for Entry<'a> {
    type Item = FieldValue<'a>;
    type IntoIter = EntryIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        EntryIter {
            entry: self,
            index: 0,
        }
    }
}

pub struct EntryIter<'a> {
    entry: Entry<'a>,
    index: usize,
}

impl<'a> Iterator for EntryIter<'a> {
    type Item = FieldValue<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.entry.fields.fields.len() {
            None
        } else {
            let result = self.entry.get(self.index);
            self.index += 1;
            Some(result)
        }
    }
}
