#[macro_use]
extern crate lazy_static;

extern crate bloom;

mod obf_bytes;
mod obf_text;
mod obf_word;

pub mod defs;

pgrx::pg_module_magic!();
