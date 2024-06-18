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

    #[must_use]
    pub fn shrink_hi(self) -> Result<Self, ShrinkHiError> {
        if self.start == self.end {
            return Err(ShrinkHiError(self.start));
        }
        Ok(Self {
            start: self.start,
            end: self.end - 1,
        })
    }

    #[must_use]
    pub fn shrink_lo(self) -> Result<Self, ShrinkLoError> {
        if self.start == self.end {
            return Err(ShrinkLoError(self.end));
        }
        Ok(Self {
            start: self.start + 1,
            end: self.end,
        })
    }

    #[must_use]
    pub fn expand_hi(self) -> Self {
        Self {
            start: self.start,
            end: self.end + 1,
        }
    }

    #[must_use]
    pub fn expand_lo(self) -> Result<Self, ExpandLoError> {
        if self.start == 0 {
            return Err(ExpandLoError);
        }
        Ok(Self {
            start: self.start - 1,
            end: self.end,
        })
    }

    #[must_use]
    pub fn merge_with(self, other: Span) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn is_none(&self) -> bool {
        // we assume spans that are starting at usize::MAX - u8::MAX are also none spans
        // because we need this condition in order to have safe NONE FixedTokenSpans
        // and the likelihood of a valid span being considered invalid because of this is close to zero
        self.start >= (Self::NONE.start - u8::MAX as usize) && self.end == Self::NONE.end
    }
}

impl GenericSpan for Span {
    #[inline(always)]
    fn start(&self) -> usize {
        self.start
    }

    #[inline(always)]
    fn end(&self) -> usize {
        self.end
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FixedTokenSpan<const SIZE: usize = 1>(pub usize);

impl<const SIZE: usize> FixedTokenSpan<SIZE> {
    // pub const NONE: FixedTokenSpan = FixedTokenSpan::new(usize::MAX - 1);

    #[inline(always)]
    pub const fn new(start: usize) -> Self {
        Self(start)
    }

    // FIXME: make this const once const traits are there!
    #[inline]
    pub fn to_unfixed_span(self) -> Span {
        Span::multi_token(self.start(), self.end())
    }

    pub const fn none() -> FixedTokenSpan<SIZE> {
        FixedTokenSpan::new(usize::MAX - SIZE)
    }
}

impl<const SIZE: usize> GenericSpan for FixedTokenSpan<SIZE> {
    #[inline(always)]
    fn start(&self) -> usize {
        self.0
    }

    #[inline]
    fn end(&self) -> usize {
        self.0 + SIZE // FIXME: should this be "self.0 - 1 + SIZE"?
    }
}

impl<const SIZE: usize> From<usize> for FixedTokenSpan<SIZE> {
    #[inline(always)]
    fn from(start: usize) -> Self {
        Self::new(start)
    }
}

pub trait GenericSpan {
    fn start(&self) -> usize;

    fn end(&self) -> usize;
}

#[derive(Debug)]
pub struct ShrinkHiError(usize);

impl Display for ShrinkHiError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("tried to shrink a span starting at ")?;
        let start = self.0.to_string();
        let start = start.as_str();
        f.write_str(start)?;
        f.write_str(" below its start")
    }
}

impl Error for ShrinkHiError {}

#[derive(Debug)]
pub struct ShrinkLoError(usize);

impl Display for ShrinkLoError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("tried to shrink a span ending at ")?;
        let end = self.0.to_string();
        let end = end.as_str();
        f.write_str(end)?;
        f.write_str(" above its end")
    }
}

impl Error for ShrinkLoError {}

#[derive(Debug)]
pub struct ExpandLoError;

impl Display for ExpandLoError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("tried to expand a span below 0")
    }
}

impl Error for ExpandLoError {}
