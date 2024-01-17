use anyhow::Error;
use crossterm::{QueueableCommand, terminal, cursor};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::marker::PhantomData;
use std::ops::{Deref, Range};
use std::sync::{Arc, Mutex, RwLock, RwLockReadGuard};

use crate::cli_core::{CLICore, Command, CommandBuilder};

use self::term::StdioTerm;
// This module is mainly used for debugging purposes

// FIXME: maybe add tab completion

pub struct CmdLineInterface<CTX> {
    windows: RwLock<Vec<Arc<Window<CTX>>>>,
}

impl<CTX> CmdLineInterface<CTX> {

    pub fn new(window: Window<CTX>) -> Self {
        Self {
            windows: RwLock::new(vec![Arc::new(window)]),
        }
    }

    pub fn push_screen(&self, window: Window<CTX>) {
        self.windows.write().unwrap().push(Arc::new(window));
        let mut lock = std::io::stdout().lock();
        lock.queue(terminal::EnterAlternateScreen);
        lock.flush();
        self.windows.read().unwrap().last().unwrap().reapply_prompt();
    }

    pub fn pop_screen(&self, ctx: &CTX) -> Option<Arc<Window<CTX>>> {
        let mut windows = self.windows.write().unwrap();
        if windows.len() > 1 {
            let mut lock = std::io::stdout().lock();
            lock.queue(terminal::LeaveAlternateScreen);
            lock.flush();
            let mut ret = windows.pop().unwrap();
            drop(windows);
            ret.handle_close(ctx);
            return Some(ret);
        }
        None
    }

    pub fn println(&self, line: &str) {
        let windows = self.windows.read().unwrap();
        windows.last().unwrap().println(line);
    }

    pub fn println_input_aligned(&self, line: &str) {
        let windows = self.windows.read().unwrap();
        windows.last().unwrap().println_input_aligned(line);
    }

    pub fn set_prompt(&self, prompt: String) {
        let windows = self.windows.read().unwrap();
        windows.last().unwrap().set_prompt(prompt);
    }

    #[inline]
    pub fn cmds(&self) -> Arc<HashMap<String, Command<CTX>>> {
        let windows = self.windows.read().unwrap();
        windows.last().unwrap().cmds().clone()
    }

    pub fn await_input(&self, ctx: &CTX) -> anyhow::Result<bool> {
        loop {
            let windows = self.windows.read().unwrap();
            let can_close = windows.len() > 1;
            let window = windows.last().unwrap().clone();
            drop(windows);
            let val = window.await_input(ctx, can_close);
            match val {
                Some(val) => return val,
                None => {
                    // FIXME: there is a race possible in here, fix this!
                    let mut windows = self.windows.write().unwrap();
                    if windows.len() > 1 {
                        let popped = windows.pop().unwrap();
                        windows.last().unwrap().reapply_prompt();
                        drop(windows);
                        popped.handle_close(ctx);
                    }
                },
            }
        }
    }

}

pub struct Cmds<'a, CTX>(RwLockReadGuard<'a, Vec<Arc<Window<CTX>>>>, &'a Window<CTX>);

impl<'a, CTX> Deref for Cmds<'a, CTX> {
    type Target = HashMap<String, Command<CTX>>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.1.cmds()
    }
}

pub struct Window<CTX> {
    core: CLICore<CTX>,
    fallback: Box<dyn FallbackHandler<CTX>>,
    term: StdioTerm,
    on_close: Box<dyn Fn(&CTX)>,
}

impl<CTX> Window<CTX> {
    pub fn new(builder: CLIBuilder<CTX>) -> Self {
        builder.build()
    }

    pub fn await_input(&self, ctx: &CTX, can_close: bool) -> Option<anyhow::Result<bool>> {
        let input = self.term.read_line_prompt(can_close);
        if input.is_none() {
            return None;
        }
        let input = input.unwrap();

        match self.core.process(ctx, input.as_str()) {
            Ok(_) => Some(Ok(true)),
            Err(err) => match err {
                crate::cli_core::InputError::ArgumentCnt { name, expected, got } => {
                    self.println(&format!("The command {} expects {} arguments but got {}", name, expected, got));
                    Some(Ok(false))
                },
                crate::cli_core::InputError::CommandNotFound { name } => Some(self.fallback.handle(input, self, ctx)),
                crate::cli_core::InputError::InputEmpty => {
                    Some(Ok(false))
                },
                crate::cli_core::InputError::ExecError { name, error } => {
                    self.println(&format!("An error occoured while executing the command \"{}\": {}", name, error));
                    Some(Ok(false))
                },
                crate::cli_core::InputError::ParamInvalidError(error) => {
                    self.println(&format!("Wrong syntax: {}", error));
                    Some(Ok(false))
                },
            },
        }
    }

    pub fn set_prompt(&self, prompt: String) {
        self.term.set_prompt(prompt);
    }

    pub fn reapply_prompt(&self) {
        self.term.reapply_prompt();
    }

    #[inline(always)]
    pub fn cmds(&self) -> &Arc<HashMap<String, Command<CTX>>> {
        self.core.cmds()
    }

    pub fn println(&self, line: &str) {
        self.term.println(line);
    }

    pub fn println_input_aligned(&self, line: &str) {
        self.term.println_aligned(line);
    }

    pub fn handle_close(&self, ctx: &CTX) {
        (&self.on_close)(ctx);
    }

}

pub trait FallbackHandler<CTX> {
    fn handle(&self, input: String, window: &Window<CTX>, ctx: &CTX) -> anyhow::Result<bool>;
}

pub struct PrintFallback<CTX>(pub String, PhantomData<CTX>);

impl<CTX> FallbackHandler<CTX> for PrintFallback<CTX> {
    fn handle(&self, input: String, window: &Window<CTX>, ctx: &CTX) -> anyhow::Result<bool> {
        window.println(format!("{}", self.0).as_str());
        Ok(false)
    }
}

pub struct CLIBuilder<CTX> {
    cmds: Vec<CommandBuilder<CTX>>,
    prompt: Option<String>,
    fallback: Option<Box<dyn FallbackHandler<CTX>>>,
    on_close: Option<Box<dyn Fn(&CTX)>>,
}

impl<CTX> CLIBuilder<CTX> {
    pub fn new() -> Self {
        Self {
            cmds: vec![],
            prompt: None,
            fallback: None,
            on_close: None,
        }
    }

    pub fn command(mut self, cmd: CommandBuilder<CTX>) -> Self {
        self.cmds.push(cmd);
        self
    }

    pub fn prompt(mut self, prompt: String) -> Self {
        self.prompt = Some(prompt);
        self
    }

    pub fn fallback(mut self, fallback: Box<dyn FallbackHandler<CTX>>) -> Self {
        self.fallback = Some(fallback);
        self
    }

    pub fn on_close(mut self, on_close: Box<dyn Fn(&CTX)>) -> Self {
        self.on_close = Some(on_close);
        self
    }

    pub fn build(self) -> Window<CTX> {
        let prompt = self.prompt.as_ref().map_or(String::new(), |prompt| format!("{}", prompt));
        Window {
            fallback: self.fallback.expect("a fallback has to be specified before a CLI can be built"),
            term: StdioTerm::new(prompt, None, 10),
            core: CLICore::new(self.cmds),
            on_close: self.on_close.unwrap_or_else(|| Box::new(|_| {})),
        }
    }
}

mod term {
    use std::{collections::HashSet, sync::{Mutex, atomic::AtomicBool}, time::Duration, io::Write};

    use crossterm::{QueueableCommand, style, cursor, event::{ poll, read, KeyModifiers }, terminal::{enable_raw_mode, self, disable_raw_mode}};
    use strip_ansi_escapes::strip_str;

    use super::{char_size, char_start};

    struct ReadCtx {
        history: Vec<String>,
        allowed_chars: Option<HashSet<char>>,
        hist_idx: usize,
        interm_hist_buffer: String,
        insert_mode: bool,
    }

    struct PrintCtx {
        buffer: String,
        prompt: String,
        prompt_len: usize,
        cursor_idx: usize,
        whole_cursor_idx: usize,
    }

    pub struct StdioTerm {
        print: Mutex<PrintCtx>,
        read: Mutex<ReadCtx>,
        reading: AtomicBool,
    }

    fn ensure_raw() {
        static RAW_MODE: AtomicBool = AtomicBool::new(false);
        if RAW_MODE.compare_exchange(false, true, std::sync::atomic::Ordering::Relaxed, std::sync::atomic::Ordering::Relaxed).is_err() {
            return;
        }
        enable_raw_mode().unwrap();
    }

    impl StdioTerm {

        pub fn new(prompt: String, allowed_chars: Option<HashSet<char>>, hist_cap: usize) -> Self {
            ensure_raw();
            let truncated = strip_str(&prompt).chars().count();
            Self {
                print: Mutex::new(PrintCtx { buffer: String::new(), prompt_len: truncated, prompt, cursor_idx: 0, whole_cursor_idx: 0 }),
                read: Mutex::new(ReadCtx { history: Vec::with_capacity(hist_cap), allowed_chars, hist_idx: 0, insert_mode: true, interm_hist_buffer: String::new() }),
                reading: AtomicBool::new(false),
            }
        }

        pub fn set_prompt(&self, prompt: String) {
            let truncated = strip_str(&prompt).chars().count();
            let mut print_ctx = self.print.lock().unwrap();
            print_ctx.prompt = prompt;
            print_ctx.prompt_len = truncated;
            self.reapply_prompt_inner(&print_ctx.prompt, &print_ctx.buffer, print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16);
        }

        pub fn reapply_prompt(&self) {
            let mut print_ctx = self.print.lock().unwrap();
            self.reapply_prompt_inner(&print_ctx.prompt, &print_ctx.buffer, print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16);
        }

        fn reapply_prompt_inner(&self, prompt: &String, buffer: &String, column: u16) {
            let mut lock = std::io::stdout().lock();
            lock.queue(cursor::MoveToColumn(0));
            lock.queue(terminal::Clear(terminal::ClearType::UntilNewLine));
            lock.queue(cursor::MoveToColumn(0));
            lock.queue(style::Print(prompt));
            lock.queue(style::Print(buffer));
            lock.queue(cursor::MoveToColumn(column));
            lock.flush();
        }

        fn println_inner<const ALIGNED: bool>(&self, val: &str) {
            let input = val.split('\n');
            let print_ctx = self.print.lock().unwrap();
            let offset = if ALIGNED {
                print_ctx.prompt_len
            } else {
                0
            };
            let mut lock = std::io::stdout().lock();
            for part in input {
                lock.queue(cursor::MoveToColumn(offset as u16));
                lock.queue(style::Print(part));
                lock.queue(terminal::Clear(terminal::ClearType::UntilNewLine));
                lock.queue(terminal::ScrollUp(1));
            }
            if self.reading.load(std::sync::atomic::Ordering::Acquire) {
                lock.queue(cursor::MoveToColumn(0));
                lock.queue(style::Print(&print_ctx.prompt));
                lock.queue(style::Print(&print_ctx.buffer));
                lock.queue(cursor::MoveToColumn(print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16));
                lock.flush();
            }
        }

        pub fn println(&self, val: &str) {
            self.println_inner::<false>(val)
        }

        pub fn println_aligned(&self, val: &str) {
            self.println_inner::<true>(val)
        }

        pub fn read_line_prompt(&self, can_leave: bool) -> Option<String> {
            let mut read_ctx = self.read.lock().unwrap();
            self.reading.store(true, std::sync::atomic::Ordering::Release);
            self.reapply_prompt();
            let ret = 'ret: loop {
                if poll(Duration::MAX).unwrap() {
                    match read().unwrap() {
                        crossterm::event::Event::FocusGained => {},
                        crossterm::event::Event::FocusLost => {},
                        crossterm::event::Event::Key(ev) => {
                            match ev.kind {
                                crossterm::event::KeyEventKind::Press | crossterm::event::KeyEventKind::Repeat => {
                                    let mut print_ctx = self.print.lock().unwrap();
                                    match ev.code {
                                        crossterm::event::KeyCode::Backspace => {
                                            if print_ctx.cursor_idx != 0 {
                                                let cursor = print_ctx.cursor_idx;
                                                let cursor = char_start(&print_ctx.buffer, cursor - 1);
                                                let size = char_size(print_ctx.buffer.as_bytes()[cursor as usize] as char);
                                                print_ctx.buffer.remove(cursor);
                                                print_ctx.cursor_idx -= size;
                                                print_ctx.whole_cursor_idx -= 1;
                                                let mut lock = std::io::stdout().lock();
                                                lock.queue(cursor::MoveToColumn(0));
                                                lock.queue(style::Print(&print_ctx.prompt));
                                                lock.queue(style::Print(&print_ctx.buffer));
                                                lock.queue(style::Print(" "));
                                                lock.queue(cursor::MoveToColumn(print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16));
                                                lock.flush();
                                            }
                                        },
                                        crossterm::event::KeyCode::Enter => {
                                            let mut lock = std::io::stdout().lock();
                                            lock.queue(terminal::ScrollUp(1));
                                            lock.queue(cursor::MoveToColumn(0));
                                            lock.flush();
                                            print_ctx.cursor_idx = 0;
                                            print_ctx.whole_cursor_idx = 0;

                                            read_ctx.interm_hist_buffer = String::new();

                                            // add to history
                                            if !print_ctx.buffer.is_empty() {
                                                if read_ctx.hist_idx == 0 {
                                                    if read_ctx.history.capacity() != 0 && read_ctx.history.len() == read_ctx.history.capacity() {
                                                        read_ctx.history.remove(0);
                                                    }
                                                    read_ctx.history.push(print_ctx.buffer.clone());
                                                } else {
                                                    let hist_len = read_ctx.history.len();
                                                    let hist_idx = read_ctx.hist_idx;
                                                    read_ctx.history[hist_len - hist_idx] = print_ctx.buffer.clone();
                                                    read_ctx.hist_idx = 0;
                                                }
                                            }

                                            let ret = core::mem::replace(&mut print_ctx.buffer, String::new());
                                            break 'ret Some(ret);
                                        },
                                        crossterm::event::KeyCode::Left => {
                                            if print_ctx.cursor_idx == 0 {
                                                continue;
                                            }
                                            let size = char_size(print_ctx.buffer.as_bytes()[print_ctx.cursor_idx as usize - 1] as char);
                                            print_ctx.cursor_idx -= size;
                                            print_ctx.whole_cursor_idx -= 1;
                                            std::io::stdout().queue(cursor::MoveLeft(1));
                                            std::io::stdout().flush();
                                        },
                                        crossterm::event::KeyCode::Right => {
                                            if print_ctx.buffer.len() == print_ctx.cursor_idx {
                                                continue;
                                            }
                                            let size = char_size(print_ctx.buffer.as_bytes()[print_ctx.cursor_idx as usize] as char);
                                            print_ctx.cursor_idx += size;
                                            print_ctx.whole_cursor_idx += 1;
                                            std::io::stdout().queue(cursor::MoveRight(1));
                                            std::io::stdout().flush();
                                        },
                                        crossterm::event::KeyCode::Up => {
                                            if read_ctx.hist_idx == read_ctx.history.len() {
                                                continue;
                                            }
                                            let buf_len = print_ctx.buffer.len();
                                            read_ctx.hist_idx += 1;
                                            let prev = core::mem::replace(&mut print_ctx.buffer, read_ctx.history[read_ctx.history.len() - read_ctx.hist_idx].clone());
                                            if read_ctx.hist_idx == 1 {
                                                read_ctx.interm_hist_buffer = prev;
                                            }
                                            print_ctx.cursor_idx = print_ctx.buffer.len();
                                            print_ctx.whole_cursor_idx = print_ctx.buffer.chars().count();
                                            let mut lock = std::io::stdout().lock();
                                            lock.queue(cursor::MoveToColumn(0));
                                            lock.queue(style::Print(" ".repeat(print_ctx.prompt_len + buf_len)));
                                            lock.queue(cursor::MoveToColumn(0));
                                            lock.queue(style::Print(&print_ctx.prompt));
                                            lock.queue(style::Print(&print_ctx.buffer));
                                            lock.flush();
                                        },
                                        crossterm::event::KeyCode::Down => {
                                            if read_ctx.hist_idx == 0 {
                                                continue;
                                            }
                                            let buf_len = print_ctx.buffer.len();
                                            read_ctx.hist_idx -= 1;
                                            if read_ctx.hist_idx != 0 {
                                                print_ctx.buffer = read_ctx.history[read_ctx.history.len() - read_ctx.hist_idx].clone();
                                            } else {
                                                print_ctx.buffer = core::mem::replace(&mut read_ctx.interm_hist_buffer, String::new());
                                            }
                                            print_ctx.cursor_idx = print_ctx.buffer.len();
                                            print_ctx.whole_cursor_idx = print_ctx.buffer.chars().count();
                                            let mut lock = std::io::stdout().lock();
                                            lock.queue(cursor::MoveToColumn(0));
                                            lock.queue(style::Print(" ".repeat(print_ctx.prompt_len + buf_len)));
                                            lock.queue(cursor::MoveToColumn(0));
                                            lock.queue(style::Print(&print_ctx.prompt));
                                            lock.queue(style::Print(&print_ctx.buffer));
                                            lock.flush();
                                        },
                                        crossterm::event::KeyCode::Home => {},
                                        crossterm::event::KeyCode::End => {},
                                        crossterm::event::KeyCode::PageUp => {},
                                        crossterm::event::KeyCode::PageDown => {},
                                        crossterm::event::KeyCode::Tab => {},
                                        crossterm::event::KeyCode::BackTab => {},
                                        crossterm::event::KeyCode::Delete => {
                                            if print_ctx.cursor_idx == print_ctx.buffer.len() {
                                                continue;
                                            }
                                            let cursor = print_ctx.cursor_idx;
                                            let cursor = char_start(&print_ctx.buffer, cursor);
                                            print_ctx.buffer.remove(cursor);
                                            let mut lock = std::io::stdout().lock();
                                            lock.queue(cursor::MoveToColumn(0));
                                            lock.queue(style::Print(&print_ctx.prompt));
                                            lock.queue(style::Print(&print_ctx.buffer));
                                            lock.queue(style::Print(" "));
                                            lock.queue(cursor::MoveToColumn(print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16));
                                            lock.flush();
                                        },
                                        crossterm::event::KeyCode::Insert => {
                                            read_ctx.insert_mode = !read_ctx.insert_mode;
                                        },
                                        crossterm::event::KeyCode::F(_) => {},
                                        crossterm::event::KeyCode::Char(chr) => {
                                            if chr == 'c' && ev.modifiers.contains(KeyModifiers::CONTROL) {
                                                let mut lock = std::io::stdout().lock();
                                                if can_leave {
                                                    lock.queue(terminal::LeaveAlternateScreen);
                                                }
                                                lock.queue(terminal::ScrollUp(1));
                                                lock.queue(cursor::MoveToColumn(0));
                                                lock.flush();
                                                disable_raw_mode().unwrap();
                                                std::process::exit(0);
                                            }
                                            if let Some(allowed_chars) = read_ctx.allowed_chars.as_ref() {
                                                if !allowed_chars.contains(&chr) {
                                                    // character not allowed
                                                    continue;
                                                }
                                            }
                                            let cursor = print_ctx.cursor_idx;
                                            if !read_ctx.insert_mode && print_ctx.cursor_idx != print_ctx.buffer.len() {
                                                print_ctx.buffer.remove(cursor);
                                            }
                                            print_ctx.buffer.insert(cursor, chr);
                                            print_ctx.cursor_idx += char_size(chr);
                                            print_ctx.whole_cursor_idx += 1;
                                            let mut lock = std::io::stdout().lock();
                                            lock.queue(cursor::MoveToColumn(0));
                                            lock.queue(style::Print(&print_ctx.prompt));
                                            lock.queue(style::Print(&print_ctx.buffer));
                                            lock.queue(cursor::MoveToColumn(print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16));
                                            lock.flush();
                                        },
                                        crossterm::event::KeyCode::Null => {},
                                        crossterm::event::KeyCode::Esc => {
                                            // only leave the screen if we can actually do so
                                            if can_leave {
                                                let mut lock = std::io::stdout().lock();
                                                lock.queue(cursor::MoveToColumn(0));
                                                lock.queue(terminal::LeaveAlternateScreen);
                                                lock.flush();
                                                return None;
                                            }
                                        },
                                        crossterm::event::KeyCode::CapsLock => {},
                                        crossterm::event::KeyCode::ScrollLock => {},
                                        crossterm::event::KeyCode::NumLock => {},
                                        crossterm::event::KeyCode::PrintScreen => {},
                                        crossterm::event::KeyCode::Pause => {},
                                        crossterm::event::KeyCode::Menu => {},
                                        crossterm::event::KeyCode::KeypadBegin => {},
                                        crossterm::event::KeyCode::Media(_) => {},
                                        crossterm::event::KeyCode::Modifier(_) => {},
                                    }
                                },
                                crossterm::event::KeyEventKind::Release => {},
                            }
                        },
                        crossterm::event::Event::Mouse(_) => {},
                        crossterm::event::Event::Paste(paste) => {
                            let mut print_ctx = self.print.lock().unwrap();
                            let mut lock = std::io::stdout().lock();
                            let cursor = print_ctx.cursor_idx;
                            print_ctx.buffer.insert_str(cursor, paste.as_str());
                            print_ctx.cursor_idx += paste.len();
                            print_ctx.whole_cursor_idx += paste.chars().count();
                            lock.queue(cursor::MoveToColumn(0));
                            lock.queue(style::Print(&print_ctx.prompt));
                            lock.queue(style::Print(&print_ctx.buffer));
                            lock.queue(cursor::MoveToColumn(print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16));
                            lock.flush();
                        },
                        crossterm::event::Event::Resize(_, _) => {},
                    }
                }
            };
            self.reading.store(false, std::sync::atomic::Ordering::Release);
            ret
        }

    }

}

#[inline]
fn char_start(src: &str, mut idx: usize) -> usize {
    const HIGH_BIT: u8 = 1 << 7;
    const DIFF_BIT: u8 = 1 << 6;

    loop {
        let raw = src.as_bytes()[idx];
        // for single byte chars
        if raw & HIGH_BIT == 0 {
            return idx;
        }
        // check if we reached the beginning
        if raw & DIFF_BIT != 0 {
            return idx;
        }
        idx -= 1;
    }
}

#[inline]
fn char_size(chr: char) -> usize {
    let chr = chr as u32;
    if chr <= 0x00007F {
        1
    } else if chr <= 0x0007FF {
        2
    } else if chr <= 0x00FFFF {
        3
    } else {
        4
    }
    // ((chr as u32).trailing_ones() as usize).max(1)
}

#[inline]
fn full_char_size(src: &str, idx: usize) -> usize {
    let start = char_start(src, idx);
    char_size(src.as_bytes()[start] as char)
}
