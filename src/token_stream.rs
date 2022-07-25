use crate::shared::{Token, TokenKind};

pub struct TokenStream {
    tokens: Box<[Token]>,
    cursor: usize,
    check_points: Vec<usize>,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_boxed_slice(),
            cursor: 0,
            check_points: vec![],
        }
    }

    pub fn get_next(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    pub fn get_next_and_advance(&mut self) -> Option<&Token> {
        self.advance();
        self.tokens.get(self.cursor - 1)
    }

    pub fn eat(&mut self, token_kind: TokenKind) -> bool {
        let next = self.get_next();
        if let Some(next) = next {
            if next.kind() == token_kind {
                self.advance();
                return true;
            }
        }
        false
    }

    #[inline]
    pub fn advance(&mut self) {
        self.cursor += 1;
    }

    pub fn skip(&mut self, steps: usize) {
        // FIXME: add validation!
        self.cursor += steps;
    }

    pub fn can_advance(&self) -> bool {
        self.tokens.len() > self.cursor
    }

    #[inline]
    pub fn go_back(&mut self) {
        self.cursor -= 1;
    }

    pub fn can_go_back(&self) -> bool {
        self.tokens.len() > self.cursor
    }

    pub fn look_ahead<F: FnOnce(&Token) -> bool>(&self, dist: usize, func: F) -> bool {
        let dist = dist.max(1) - 1;
        if let Some(token) = self.tokens.get(self.cursor + dist) {
            func(token)
        } else {
            // FIXME: is this behavior correct? - we should, at the very least document it!
            false
        }
    }

    pub fn look_back<T, F: FnOnce(&Token) -> T>(&self, dist: usize, func: F) -> Option<T> {
        let dist = dist + 1;
        if self.cursor < dist {
            return None;
        }
        if let Some(token) = self.tokens.get(self.cursor - dist) {
            Some(func(token))
        } else {
            None
        }
    }

    pub(crate) fn internal_tokens(&self) -> Box<[Token]> {
        self.tokens.clone()
    }

    pub(crate) fn replace_internal_tokens(&mut self, new_toks: Box<[Token]>) {
        self.tokens = new_toks;
    }

    #[inline]
    pub fn cursor(&self) -> usize {
        self.cursor - 1
    }

    #[inline]
    pub fn reset(&mut self) {
        self.cursor = 0;
        self.check_points.clear();
    }

    pub fn push_checkpoint(&mut self) {
        self.check_points.push(self.cursor);
    }

    pub fn pop_checkpoint(&mut self) -> bool {
        self.check_points.pop().is_some()
    }

    pub fn clear_checkpoints(&mut self) {
        self.check_points.clear();
    }

    pub fn go_back_to_last_checkpoint(&mut self) -> bool {
        let last = self.check_points.pop();
        if let Some(last) = last {
            self.cursor = last;
            true
        } else {
            false
        }
    }

    pub fn go_back_to_last_checkpoint_and_insert_checkpoint(&mut self) -> bool {
        let curr = self.cursor;
        let last = self.check_points.pop();
        self.check_points.push(curr);
        if let Some(last) = last {
            self.cursor = last;
            true
        } else {
            false
        }
    }
}
