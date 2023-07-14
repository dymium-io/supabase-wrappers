use bloom::{BloomFilter, ASMS};
use rand::prelude::*;
use std::collections::HashMap;
use std::sync::Mutex;

pub(crate) fn obf(s: &str) -> String {
    let mut obf_mapper = OBF_MAPPER.lock().unwrap();
    obf_mapper.insert(s)
}

lazy_static! {
    static ref OBF_MAPPER: Mutex<Mapping> = Mutex::new(Mapping::new());
}

struct Mapping {
    forward: HashMap<Box<str>, String>,
    reverse: HashMap<Box<str>, String>,
    bloom: BloomFilter,
}

impl Mapping {
    fn new() -> Self {
        let mut m = Self {
            forward: HashMap::new(),
            reverse: HashMap::new(),
            bloom: BloomFilter::with_rate(0.01, 10000),
        };
        let mut rng = rand::thread_rng();
        let mut prefill = |chars: Vec<char>| {
            let mut o_chars = chars.clone();
            o_chars.shuffle(&mut rng);
            chars.iter().zip(o_chars.iter()).for_each(|(c, o)| {
                m.forward.insert(c.to_string().into(), o.to_string());
                m.forward.insert(o.to_string().into(), c.to_string());
                m.bloom.insert(&o.to_string());
            });
        };
        prefill(('0'..='9').collect());
        prefill(('a'..='z').collect());
        m
    }

    fn insert(&mut self, s: &str) -> String {
        let lowered = LowerCase::<1024>::from(s);
        if let Some(obfuscated) = self.forward.get(lowered.as_str()) {
            return obfuscated.clone().capitalize(s);
        }

        let mut obfuscated = String::with_capacity(s.len());
        obfuscated.gen(s);
        if s.len() == 1 {
            obfuscated.gen(s);
        } else {
            for _ in 0..100 {
                obfuscated.gen(s);
                if !self.bloom.contains(&obfuscated)
                    || !self.reverse.contains_key(obfuscated.as_str())
                {
                    break;
                }
            }
        }
        self.forward
            .insert(lowered.as_str().to_string().into(), obfuscated.clone());
        self.reverse
            .insert(obfuscated.clone().into(), lowered.to_string());
        self.bloom.insert(&obfuscated);
        obfuscated.capitalize(s)
    }
}

trait Obf {
    fn gen(&mut self, s: &str);
    fn capitalize(self, s: &str) -> Self;
}

impl Obf for String {
    fn gen(&mut self, s: &str) {
        let mut rng = rand::thread_rng();
        self.clear();
        s.chars().for_each(|c| {
            self.push(match c {
                '-' | '_' => c,
                '0'..='9' => (b'0' + rng.gen_range(0..10)) as char,
                _ => (b'a' + rng.gen_range(0..26)) as char,
            })
        });
    }
    fn capitalize(mut self, s: &str) -> Self {
        let bytes = unsafe { self.as_bytes_mut() };
        s.chars().zip(bytes).for_each(|(c, o)| {
            if c.is_uppercase() {
                o.make_ascii_uppercase()
            }
        });
        self
    }
}

enum LowerCase<const N: usize> {
    OnStack(heapless::String<N>),
    OnHeap(String),
}

impl<const N: usize> LowerCase<N> {
    fn from(s: &str) -> Self {
        if s.len() <= N / 4 {
            let mut ss = heapless::String::<N>::new();
            s.chars()
                .for_each(|c| c.to_lowercase().for_each(|cc| ss.push(cc).unwrap()));
            LowerCase::OnStack(ss)
        } else {
            LowerCase::OnHeap(s.to_lowercase())
        }
    }

    fn as_str(&self) -> &str {
        match self {
            LowerCase::OnStack(s) => s.as_str(),
            LowerCase::OnHeap(s) => s.as_str(),
        }
    }

    fn to_string(self) -> String {
        match self {
            LowerCase::OnStack(s) => s.as_str().to_string(),
            LowerCase::OnHeap(s) => s,
        }
    }
}
