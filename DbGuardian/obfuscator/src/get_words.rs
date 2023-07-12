pub(crate) struct GetWords {
    s: String,
    p: fn(u8) -> bool,
}

impl GetWords {
    pub(crate) fn new(inp: &str, predicate: fn(u8) -> bool) -> Self {
        Self {
            s: String::from(inp),
            p: predicate,
        }
    }

    pub(crate) fn to_str(self) -> String {
        self.s
    }

    pub(crate) fn iter(&mut self) -> GetWordsIterator {
        GetWordsIterator {
            res: &mut self.s,
            index: 0,
            predicate: &self.p,
        }
    }
}

pub(crate) struct GetWordsIterator<'a> {
    res: &'a mut String,
    index: usize,
    predicate: &'a fn(u8) -> bool,
}

impl<'a> Iterator for GetWordsIterator<'a> {
    type Item = &'a mut [u8];

    fn next(&mut self) -> Option<Self::Item> {
        let binp = self.res.as_bytes();
        let len = self.res.len();
        if self.index == len {
            return None;
        }
        let b = self.index;
        self.index = len; // worst case: no match found
        for k in b..len {
            if (self.predicate)(binp[k]) {
                self.index = k;
                break;
            }
        }
        if self.index == len {
            return None;
        }
        let b = self.index;
        self.index = len; // best case: everything matches!
        for k in b..len {
            if !(self.predicate)(binp[k]) {
                self.index = k;
                break;
            }
        }
        let p = self.res.as_mut_ptr();
        let r = unsafe { std::slice::from_raw_parts_mut(p.offset(b as isize), self.index - b) };
        Some(r)
    }
}
