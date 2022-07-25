use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize, // FIXME: should this be inclusive or exclusive?
}

impl Span {
    pub const NONE: Span = Span {
        start: usize::MAX,
        end: usize::MAX,
    };

    pub fn single_token(position: usize) -> Self {
        Self {
            start: position,
            end: position + 1, // FIXME: should this be `position`?
        }
    }

    pub fn fixed_token<const SIZE: usize>(position: usize) -> Self {
        Self {
            start: position,
            end: position + SIZE, // FIXME: should this be `position - 1 + SIZE`?
        }
    }

    #[inline]
    pub fn multi_token(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn shrink_hi(&mut self) -> Result<(), ShrinkHiError> {
        if self.start == self.end {
            return Err(ShrinkHiError(self.start));
        }
        self.end = self.end - 1;
        Ok(())
    }

    pub fn shrink_lo(&mut self) -> Result<(), ShrinkLoError> {
        if self.start == self.end {
            return Err(ShrinkLoError(self.end));
        }
        self.end = self.end - 1;
        Ok(())
    }

    pub fn expand_hi(&mut self) {
        self.end += 1;
    }

    pub fn is_none(&self) -> bool {
        self.start == Self::NONE.start && self.end == Self::NONE.end
    }
}

impl GenericSpan for Span {
    #[inline]
    fn start(&self) -> usize {
        self.start
    }

    #[inline]
    fn end(&self) -> usize {
        self.end
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FixedTokenSpan<const SIZE: usize = 1>(pub usize); // FIXME: make use of FixedTokenSpan

impl<const SIZE: usize> FixedTokenSpan<SIZE> {
    #[inline]
    pub const fn new(position: usize) -> Self {
        Self(position)
    }
}

impl<const SIZE: usize> GenericSpan for FixedTokenSpan<SIZE> {
    #[inline]
    fn start(&self) -> usize {
        self.0
    }

    #[inline]
    fn end(&self) -> usize {
        self.0 + SIZE // FIXME: should this be "self.0 - 1 + SIZE"?
    }
}

pub trait GenericSpan {
    fn start(&self) -> usize;

    fn end(&self) -> usize;
}

pub struct ShrinkHiError(usize);

impl Debug for ShrinkHiError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("tried to shrink a span starting at ")?;
        let start = self.0.to_string();
        let start = start.as_str();
        f.write_str(start)?;
        f.write_str(" below its start")
    }
}

impl Display for ShrinkHiError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let dbg = format!("{:?}", self);
        f.write_str(dbg.as_str())
    }
}

impl Error for ShrinkHiError {}

pub struct ShrinkLoError(usize);

impl Debug for ShrinkLoError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("tried to shrink a span ending at ")?;
        let end = self.0.to_string();
        let end = end.as_str();
        f.write_str(end)?;
        f.write_str(" above its end")
    }
}

impl Display for ShrinkLoError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let dbg = format!("{:?}", self);
        f.write_str(dbg.as_str())
    }
}

impl Error for ShrinkLoError {}
