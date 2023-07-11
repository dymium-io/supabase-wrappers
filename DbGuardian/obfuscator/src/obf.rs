use typenum::U16;

use aes::cipher::{generic_array::GenericArray, BlockEncrypt, KeyInit};
use aes::Aes128;

pub(crate) fn obf(buf: &mut [u8]) {
    lazy_static! {
        static ref KEY: GenericArray<u8, U16> = GenericArray::from([
            0xc4, 0x6c, 0xdc, 0xa3, 0x6d, 0xe3, 0x4c, 0x08, 0x0b, 0x37, 0xee, 0xfa, 0x3d, 0x62,
            0x51, 0x39
        ]);
    }
    let cypher = Aes128::new(&KEY);
    let mut block = GenericArray::from([0u8; 16]);

    let nb = buf.len() / 16;
    for kb in 0..=nb {
        let lb = if kb == nb { buf.len() % 16 } else { 16 };
        if lb == 0 {
            break;
        }
        for k in 0..lb {
            let kk = kb * 16 + k;
            block[k] ^= buf[kk];
        }
        cypher.encrypt_block(&mut block);
        for k in 0..lb {
            let kk = kb * 16 + k;
            if buf[kk].is_ascii_lowercase() || buf[kk] >= 128 {
                buf[kk] = b'a' + ((buf[kk] as u16 + block[k] as u16) % 26) as u8;
            } else if buf[kk].is_ascii_uppercase() {
                buf[kk] = b'A' + ((buf[kk] as u16 + block[k] as u16) % 26) as u8;
            } else if buf[kk].is_ascii_digit() {
                buf[kk] = b'0' + ((buf[kk] as u16 + block[k] as u16) % 10) as u8;
            }
        }
    }
}

pub(crate) fn obf_bytes(buf: &mut [u8]) {
    lazy_static! {
        static ref KEY: GenericArray<u8, U16> = GenericArray::from([
            0xc4, 0x6c, 0xdc, 0xa3, 0x6d, 0xe3, 0x4c, 0x08, 0x0b, 0x37, 0xee, 0xfa, 0x3d, 0x62,
            0x51, 0x39
        ]);
    }
    let cypher = Aes128::new(&KEY);
    let mut block = GenericArray::from([0u8; 16]);

    let nb = buf.len() / 16;
    for kb in 0..=nb {
        let lb = if kb == nb { buf.len() % 16 } else { 16 };
        if lb == 0 {
            break;
        }
        for k in 0..lb {
            let kk = kb * 16 + k;
            block[k] ^= buf[kk];
        }
        cypher.encrypt_block(&mut block);
        for k in 0..lb {
            let kk = kb * 16 + k;
            buf[kk] = block[k];
        }
    }
}
