use crate::span::Span;
use colored::*;
use std::error::Error;
use std::fmt::{Display, Formatter};

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
        ret.error_spanned(error, span);
        ret
    }

    #[inline]
    pub(crate) fn from_input_and_err(input: String, error: String) -> Self {
        Self::from_input_and_err_with_span(input, error, Span::NONE)
    }

    pub fn error_spanned(&mut self, error: String, span: Span) -> &mut Self {
        self.items.push(DiagnosticItem::Error(error, span));
        self
    }

    #[inline]
    pub fn error(&mut self, error: String) -> &mut Self {
        self.error_spanned(error, Span::NONE)
    }

    pub fn warn_spanned(&mut self, warning: String, span: Span) -> &mut Self {
        self.items.push(DiagnosticItem::Warn(warning, span));
        self
    }

    #[inline]
    pub fn warn(&mut self, warning: String) -> &mut Self {
        self.warn_spanned(warning, Span::NONE)
    }

    pub fn note(&mut self, note: String) -> &mut Self {
        self.items.push(DiagnosticItem::Note(note));
        self
    }

    pub fn suggest_spanned(&mut self, suggestion: String, span: Span) -> &mut Self {
        self.items
            .push(DiagnosticItem::Suggestion(suggestion, span));
        self
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
    Warn(String, Span),
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
            DiagnosticItem::Warn(str, span) => {
                if !span.is_none() {
                    input.to_owned()
                        + "\n"
                        + &DiagnosticBuilder::build_span_string(span)
                            .yellow()
                            .to_string()
                        + "\n"
                        + &" ".repeat(span.start)
                        + &str.yellow().to_string()
                        + "\n"
                } else {
                    input.to_owned() + "\n" + &str.yellow().to_string() + "\n"
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
            Span::single_token($sp),
        ))
    };
    ($input:expr, $error:expr, $sp:expr) => {
        Err(DiagnosticBuilder::from_input_and_err_with_span(
            $input,
            $error,
            Span::single_token($sp),
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
