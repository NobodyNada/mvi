mod pattern;
use std::{
    rc::Rc,
    sync::atomic::{AtomicU64, Ordering},
};

pub use pattern::*;

use crate::tas::Tas;

/// An edit to the TAS.
///
/// All modifications to the TAS are performed by obtaining a handle to an Action, modifying the
/// action, and then committing it. The most recent Action can be modified through its handle as
/// the user makes edits (without needing to pushing new edits to the undo stack). Starting another
/// action will immediately commit the current action, invalidating any existing handle.
///
/// Actions are always reversible and repeatable.
#[derive(Debug)]
pub struct Action {
    /// The location of the edit.
    pub cursor: u32,

    /// What the edit is.
    pub kind: ActionKind,

    /// The action's identifier, used to detect when a handle is invalidated.
    id: u64,

    /// Whether this action is eligible for the repeat command.
    pub repeatable: bool,
}

#[derive(Debug, Clone)]
pub enum ActionKind {
    /// Insert the given frames at the cursor.
    Insert(Vec<u8>),
    /// Delete the given frames at the cursor.
    Delete(Vec<u8>),
    /// Apply a given pattern to the given contents, starting at the cursor.
    Apply { pattern: Pattern, previous: Vec<u8> },
}

#[derive(Debug)]
pub struct ActionHandle {
    id: Option<u64>,
    repeatable: bool,
}

static NEXT_ID: AtomicU64 = AtomicU64::new(0);

impl Action {
    /// Returns true if this action is blank.
    pub fn is_empty(&self) -> bool {
        match &self.kind {
            ActionKind::Insert(frames)
            | ActionKind::Delete(frames)
            | ActionKind::Apply {
                previous: frames, ..
            } => frames.is_empty(),
        }
    }

    pub(super) fn undo(&self, tas: &mut Tas) {
        tas.invalidate(self.cursor);
        let start = self.cursor as usize * tas.movie.frame_size();
        match &self.kind {
            ActionKind::Insert(frames) => {
                let end = start + frames.len();
                tas.movie.data.drain(start..end);
            }
            ActionKind::Delete(frames) => {
                tas.movie.data.splice(start..start, frames.iter().cloned());
            }
            ActionKind::Apply {
                pattern: _,
                previous,
            } => {
                tas.movie.data[start..start + previous.len()].copy_from_slice(previous);
            }
        }
        tas.select(self.cursor.min(tas.movie.len().saturating_sub(1)));
    }

    pub(super) fn redo(&self, tas: &mut Tas) {
        tas.invalidate(self.cursor);
        let start = self.cursor as usize * tas.movie.frame_size();
        match &self.kind {
            ActionKind::Insert(frames) => {
                tas.movie.data.splice(start..start, frames.iter().cloned());
            }
            ActionKind::Delete(frames) => {
                let end = start + frames.len();
                tas.movie.data.drain(start..end);
            }
            ActionKind::Apply { pattern, previous } => {
                pattern.apply(
                    &tas.movie.input_ports,
                    &mut tas.movie.data[start..start + previous.len()],
                );
            }
        }
        tas.select(self.cursor.min(tas.movie.len().saturating_sub(1)));
    }
}

impl Clone for Action {
    fn clone(&self) -> Self {
        Self {
            cursor: self.cursor,
            kind: self.kind.clone(),
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            repeatable: self.repeatable,
        }
    }
}

impl ActionHandle {
    /// Inserts frames before the specified index.
    pub fn insert(&mut self, tas: &mut Tas, idx: u32, buf: &[u8]) {
        let size = tas.movie.frame_size();
        assert_eq!(buf.len() % size, 0);

        if let Some(Action {
            cursor,
            kind: ActionKind::Insert(existing),
            ..
        }) = self.resolve(tas)
            && *cursor + (existing.len() / size) as u32 == idx
        {
            existing.extend_from_slice(buf);
        } else {
            self.push(tas, idx, ActionKind::Insert(buf.to_owned()));
        }

        tas.invalidate(idx);
        let insert_idx = idx as usize * size;
        tas.movie.ensure_length(idx);
        tas.movie
            .data
            .splice(insert_idx..insert_idx, buf.iter().cloned());
    }

    /// Inserts a number of blank frames before the specified index.
    #[allow(unused)]
    pub fn insert_blank(&mut self, tas: &mut Tas, idx: u32, len: usize) {
        self.insert(
            tas,
            idx,
            &std::iter::repeat_n(0, len * tas.movie().frame_size()).collect::<Vec<u8>>(),
        );
    }

    pub fn delete(&mut self, tas: &mut Tas, frames: impl std::ops::RangeBounds<u32>) {
        let size = tas.movie().frame_size();
        let range = (
            match frames.start_bound() {
                std::ops::Bound::Unbounded => std::ops::Bound::Unbounded,
                std::ops::Bound::Included(frame) => {
                    std::ops::Bound::Included(*frame as usize * size)
                }
                std::ops::Bound::Excluded(frame) => {
                    std::ops::Bound::Excluded(*frame as usize * size + (size - 1))
                }
            },
            match frames.end_bound() {
                std::ops::Bound::Unbounded => std::ops::Bound::Unbounded,
                std::ops::Bound::Included(frame) => {
                    std::ops::Bound::Included(*frame as usize * size + (size - 1))
                }
                std::ops::Bound::Excluded(frame) => {
                    std::ops::Bound::Excluded(*frame as usize * size)
                }
            },
        );
        let start = match frames.start_bound() {
            std::ops::Bound::Unbounded => 0,
            std::ops::Bound::Excluded(frame) => frame + 1,
            std::ops::Bound::Included(frame) => *frame,
        };

        let mut data: Vec<u8> = tas.movie.data.drain(range).collect();
        match self.resolve(tas) {
            Some(Action {
                cursor,
                kind: ActionKind::Delete(buf),
                ..
            }) if *cursor + (buf.len() / size) as u32 == start => {
                buf.append(&mut data);
            }
            Some(Action {
                cursor,
                kind: ActionKind::Delete(buf),
                ..
            }) if start + (data.len() / size) as u32 == *cursor => {
                data.append(buf);
                *buf = data;
            }
            _ => {
                self.push(tas, start, ActionKind::Delete(data));
            }
        }
        tas.invalidate(start);
    }

    pub fn replace_frame(&mut self, tas: &mut Tas, idx: u32, new: &[u8]) {
        let size = tas.movie.frame_size();
        assert_eq!(new.len(), size);
        let mut data = tas.frame(idx).to_owned();

        let action = self.resolve(tas);
        if let Some(Action {
            cursor,
            kind: ActionKind::Apply { pattern, previous },
            ..
        }) = action
            && (cursor.saturating_sub(1)..=(*cursor + pattern.len() as u32)).contains(&idx)
        {
            let len = pattern.len();
            let buf = Rc::make_mut(&mut pattern.buf);
            assert_eq!(pattern.offset, 0);
            assert!(buf.autofire.is_none() && buf.autohold.is_none() && buf.mask.is_none());

            if *cursor != 0 && idx == (*cursor - 1) {
                // Prepend the new frame to the existing action.
                *cursor = idx;
                buf.data = new
                    .iter()
                    .copied()
                    .chain(buf.data.iter().copied())
                    .collect();
                data.append(previous);
                *previous = data;
            } else if idx == *cursor + (len as u32) {
                // Append the new frame to the existing action.
                previous.append(&mut data);
                buf.data.extend_from_slice(new);
            } else {
                // Replace the existing frame in the existing action.
                let offset = (idx - *cursor) as usize * size;
                buf.data
                    .splice(offset..(offset + size), new.iter().copied());
            }
        } else if let Some(Action {
            cursor,
            kind: ActionKind::Insert(buf),
            ..
        }) = action
            && (*cursor..*cursor + (buf.len() / size) as u32).contains(&idx)
        {
            let offset = (idx - *cursor) as usize * size;
            buf[offset..][..size].copy_from_slice(new);
        } else {
            self.push(
                tas,
                idx,
                ActionKind::Apply {
                    pattern: Pattern {
                        buf: Rc::new(PatternBuf {
                            data: new.to_owned(),
                            frame_size: size,
                            mask: None,
                            autofire: None,
                            autohold: None,
                        }),
                        offset: 0,
                    },
                    previous: data,
                },
            );
        }
        tas.movie.frame_mut(idx).copy_from_slice(new);
    }

    fn resolve<'a>(&'a mut self, tas: &'a mut Tas) -> Option<&'a mut Action> {
        tas.undo_history
            .last_mut()
            .filter(|a| Some(a.id) == self.id)
    }

    fn push(&mut self, tas: &mut Tas, cursor: u32, action: ActionKind) {
        tas.push_undo(Action {
            cursor,
            kind: action,
            id: *self
                .id
                .get_or_insert_with(|| NEXT_ID.fetch_add(1, Ordering::Relaxed)),
            repeatable: self.repeatable,
        });
    }

    pub(super) fn new(repeatable: bool) -> Self {
        ActionHandle {
            id: None,
            repeatable,
        }
    }
}
