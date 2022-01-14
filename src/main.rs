
mod eval;
mod graphics;

use crate::eval::_lib::{Config, DiagnosticsConfig};
use colored::*;

pub const MODE: Mode = Mode::Console;

fn main() {
    match MODE {
        Mode::Console => {
            crate::eval::main::main();
        },
        Mode::Gui => {}
    }
}

pub enum Mode {
    Console,
    Gui,
}