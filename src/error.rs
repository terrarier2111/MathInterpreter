use colored::*;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub const NONE: Span = Span::new(usize::MAX, usize::MAX);

    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub const fn from_idx(idx: usize) -> Self {
        if idx == usize::MAX {
            Self::NONE
        } else {
            Self::new(idx, idx + 1)
        }
    }

    #[inline]
    pub const fn is_none(&self) -> bool {
        self.end == usize::MAX && self.start == usize::MAX
    }

    #[inline]
    pub fn shrink_lo(&mut self) {
        if self.end - self.start > 1 {
            self.start += 1;
        } else {
            // TODO: Error properly!
        }
    }

    #[inline]
    pub fn shrink_hi(&mut self) {
        if self.end - self.start > 1 {
            self.end -= 1;
        } else {
            // TODO: Error properly!
        }
    }

    #[inline]
    pub fn expand_lo(&mut self) {
        // TODO: Should this be renamed to grow_*?
        if self.start > 0 {
            self.start -= 1;
        } else {
            // TODO: Error properly!
        }
    }

    #[inline]
    pub fn expand_hi(&mut self) {
        // TODO: Should this be renamed to grow_*?
        self.end += 1;
    }
}

#[derive(Debug)]
pub struct DiagnosticBuilder {
    input: String,
    items: Vec<DiagnosticItem>,
}

impl DiagnosticBuilder {
    pub fn new(input: String) -> Self {
        Self {
            input,
            items: vec![],
        }
    }

    pub(crate) fn from_input_and_err_with_span(input: String, error: String, span: Span) -> Self {
        let mut ret = Self {
            input,
            items: vec![],
        };
        ret.error_with_span(error, span);
        ret
    }

    pub(crate) fn from_input_and_err(input: String, error: String) -> Self {
        let mut ret = Self {
            input,
            items: vec![],
        };
        ret.error(error);
        ret
    }

    pub fn note(&mut self, note: String) -> &mut Self {
        self.items.push(DiagnosticItem::Note(note));
        self
    }

    pub fn error_with_span(&mut self, error: String, span: Span) -> &mut Self {
        self.items.push(DiagnosticItem::Error(error, span));
        self
    }

    pub fn error(&mut self, error: String) -> &mut Self {
        self.items.push(DiagnosticItem::Error(error, Span::NONE));
        self
    }

    pub fn suggestion_with_span(&mut self, suggestion: String, span: Span) -> &mut Self {
        self.items
            .push(DiagnosticItem::Suggestion(suggestion, span));
        self
    }

    pub fn build_msg(&self, msg: String) -> Vec<String> {
        let mut result = vec![];
        result.push(msg);

        result
    }

    fn build_span_string(span: &Span) -> String {
        " ".repeat(span.start) + &*"^".repeat(span.end - span.start)
    }

    fn build_multi_span_string(len: usize, spans: &Vec<Span>) -> String {
        let mut result = " ".repeat(len);
        for span in spans {
            result.replace_range(span.start..span.end, &*"^".repeat(span.end - span.start));
        }
        result
    }
}

impl Display for DiagnosticBuilder {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut combined = String::new();
        for x in self.items.iter() {
            combined.push_str(&*x.to_string(&self.input));
        }
        f.write_str(&*combined)
    }
}

impl Error for DiagnosticBuilder {}

#[derive(Debug)]
pub(crate) enum DiagnosticItem {
    Error(String, Span),
    Suggestion(String, Span),
    Note(String),
}

impl DiagnosticItem {
    pub fn to_string(&self, input: &String) -> String {
        match self {
            DiagnosticItem::Error(str, span) => {
                if !span.is_none() {
                    input.to_owned()
                        + "\n"
                        + &DiagnosticBuilder::build_span_string(span).red().to_string()
                        + "\n"
                        + &" ".repeat(span.start)
                        + &str.red().to_string()
                        + "\n"
                } else {
                    input.to_owned() + "\n" + &str.red().to_string() + "\n"
                }
            }
            DiagnosticItem::Suggestion(str, span) => {
                if !span.is_none() {
                    input.to_owned()
                        + "\n"
                        + &DiagnosticBuilder::build_span_string(span)
                        + "\n"
                        + &" ".repeat(span.start)
                        + str
                        + "\n"
                } else {
                    input.to_owned() + "\n" + str + "\n"
                }
            }
            DiagnosticItem::Note(str) => String::from("note: ") + str + "\n",
        }
    }
}

#[macro_export]
macro_rules! diagnostic_builder {
    ($input:expr, $error:literal) => {
        Err(DiagnosticBuilder::from_input_and_err(
            $input,
            $error.to_string(),
        ))
    };
    ($input:expr, $error:expr) => {
        Err(DiagnosticBuilder::from_input_and_err($input, $error))
    };
    ($input:expr, $error:literal, $sp:expr) => {
        Err(DiagnosticBuilder::from_input_and_err_with_span(
            $input,
            $error.to_string(),
            Span::from_idx($sp),
        ))
    };
    ($input:expr, $error:expr, $sp:expr) => {
        Err(DiagnosticBuilder::from_input_and_err_with_span(
            $input,
            $error,
            Span::from_idx($sp),
        ))
    };
}

#[macro_export]
macro_rules! diagnostic_builder_spanned {
    ($input:expr, $error:literal, $sp:expr) => {
        Err(DiagnosticBuilder::from_input_and_err_with_span(
            $input,
            $error.to_string(),
            $sp,
        ))
    };
    ($input:expr, $error:expr, $sp:expr) => {
        Err(DiagnosticBuilder::from_input_and_err_with_span(
            $input, $error, $sp,
        ))
    };
}
