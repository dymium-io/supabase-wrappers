#[macro_use]
extern crate lazy_static;

use pgx::datum::Uuid;
use pgx::prelude::*;

mod get_words;
mod obf;

pgx::pg_module_magic!();

#[pg_extern(name = "_ab44f4901b2ebca16067351bbd3ce6060a3e8c7d20fd6dc9ba21b49a_")]
fn obfuscate_text(key: i32, txt: &'static str, _sz: i32, _pad: bool) -> String {
    if key != 0x226193b1 {
        error!("Internal error");
    }
    let mut gw = get_words::GetWords::new(txt, |x| {
        x.is_ascii_alphanumeric() || x == b'_' || x == b'-' || x >= 128
    });
    gw.iter().for_each(obf::obf);
    gw.to_str()
}

#[pg_extern(name = "_d0f1ebe1531fe33abc4245b3e63949cafa7f1999828c2ea71c55d795_")]
fn obfuscate_uuid(key: i32, uuid: Uuid) -> Uuid {
    if key != 0x226193b1 {
        error!("Internal error");
    }
    let mut u = uuid.clone();
    obf::obf_bytes(u.as_mut_slice());
    u
}

#[pg_extern(name = "_a4bb1a46bb495f7c954f5f50ac1c8f41a0d5a24326f2a89ff076c4c8_")]
fn obfuscate_text_array(key: i32, txt: Vec<&'static str>, _sz: i32, _pad: bool) -> Vec<String> {
    if key != 0x226193b1 {
        error!("Internal error");
    }
    txt.iter()
        .map(|&t| {
            let mut gw = get_words::GetWords::new(t, |x| {
                x.is_ascii_alphanumeric() || x == b'_' || x == b'-' || x >= 128
            });
            gw.iter().for_each(obf::obf);
            gw.to_str()
        })
        .collect::<Vec<_>>()
}

#[pg_extern(name = "_4da4781e67adf5ee7fd4ef1ed794406cc4bcf07df7d3cfc704685491_")]
fn obfuscate_uuid_array(key: i32, uuid: Vec<Uuid>) -> Vec<Uuid> {
    if key != 0x226193b1 {
        error!("Internal error");
    }
    uuid.iter()
        .map(|&u| {
            let mut uu = u.clone();
            obf::obf_bytes(uu.as_mut_slice());
            uu
        })
        .collect::<Vec<_>>()
}
