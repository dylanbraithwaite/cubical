mod util;
mod syntax;
mod normalise;
mod ast;
mod var_target;
mod toplevel;
mod interval;
mod context;
mod elaborate;
mod face;
mod subst;
mod debruijn;
mod typeinf;

use std::io::{BufRead, stdin};

fn main() {
    let stdin = stdin();
    let mut ctx = context::Context::new();

    for line in stdin.lock().lines() {
        if let Ok(line) = line {
            match toplevel::eval::execute_command(&mut ctx, &line) {
                Ok(s) => println!("{}", s),
                Err(e) => println!("{:?}", e),
            }
        }
    }
}
