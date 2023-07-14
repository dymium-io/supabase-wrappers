use once_cell::sync::Lazy;
use regex::Regex;

use crate::obf_word;

static WORD_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"(\b\w+(?:-\w+)*\b)").unwrap());

pub(crate) fn obf(txt: &str) -> String {
    let mut result = String::with_capacity(txt.len());
    let mut last_match_end = 0;
    for word in WORD_REGEX.find_iter(txt) {
        // Add the text from after the last match to the start of this match
        result.push_str(&txt[last_match_end..word.start()]);
        result.push_str(&obf_word::obf(word.as_str()));
        last_match_end = word.end();
    }
    // Append the rest of the string
    result.push_str(&txt[last_match_end..]);

    result
}
