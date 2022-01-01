#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {

    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
        }
    }

}

pub(crate) struct DiagnosticBuilder {
    items: Vec<DiagnosticItem>,
}

impl DiagnosticBuilder {
    
    pub fn new() -> Self {
        Self {
            items: vec![],
        }
    }

    pub fn note(&mut self, note: String) -> &mut Self {
        self.items.push(DiagnosticItem::Note(note));
        self
    }

    pub fn error(&mut self, error: String, span: Span) -> &mut Self {
        self.items.push(DiagnosticItem::Error(error, span));
        self
    }

    pub fn suggestion(&mut self, suggestion: String, span: Span) -> &mut Self {
        self.items.push(DiagnosticItem::Suggestion(suggestion, span));
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

pub(crate) enum DiagnosticItem {

    Error(String, Span),
    Suggestion(String, Span),
    Note(String),

}

impl DiagnosticItem {

    pub fn to_string(&self, input: &String) -> String {
        match self {
            DiagnosticItem::Error(str, span) => {
                input.to_owned() + "\n" + &DiagnosticBuilder::build_span_string(span) + "\n" + &" ".repeat(span.start) + str
            },
            DiagnosticItem::Suggestion(str, span) => {
                input.to_owned() + "\n" + &DiagnosticBuilder::build_span_string(span) + "\n" + &" ".repeat(span.start) + str
            },
            DiagnosticItem::Note(str) => {
                String::from("note: ") + str
            },
        }
    }

}