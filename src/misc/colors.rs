#![allow(unused_macros)]
use crate::global;

global!(RESET: &'static str = "\x1b[0m", get_RESET);
global!(BOLD: &'static str = "\x1b[1m", get_BOLD);
global!(UNDERLINE: &'static str = "\x1b[4m", get_UNDERLINE);
global!(RED: &'static str = "\x1b[31m", get_RED);
global!(GREEN: &'static str = "\x1b[32m", get_GREEN);
global!(YELLOW: &'static str = "\x1b[33m", get_YELLOW);
global!(BLUE: &'static str = "\x1b[34m", get_BLUE);
global!(MAGENTA: &'static str = "\x1b[35m", get_MAGENTA);
global!(CYAN: &'static str = "\x1b[36m", get_CYAN);
global!(WHITE: &'static str = "\x1b[37m", get_WHITE);

macro_rules! disable_colors {
    () => {
        unsafe {
            RESET = Some("");
            BOLD = Some("");
            UNDERLINE = Some("");
            RED = Some("");
            GREEN = Some("");
            YELLOW = Some("");
            BLUE = Some("");
            MAGENTA = Some("");
            CYAN = Some("");
            WHITE = Some("");
        }
    };
}

pub(crate) use disable_colors;
