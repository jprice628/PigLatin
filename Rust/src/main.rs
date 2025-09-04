mod tokens;
mod lexer;
mod pigifier;

use crate::lexer::lex;
use crate::pigifier::pigify;

fn main() {
    let text = r#"Hello there! This is a simple test of the Pig Latin converter. It should handle punctuation, capitalization, and even tricky words like "xylophone" or "rhythm." Don't forget contractions -- those apostrophes matter. Also, consider how to treat acronyms like NASA or abbreviations such as e.g., i.e., and etc. Finally, test how it deals with numbers: 42 apples, 3.14 pies, and 100% effort!"#;

    let mut tokens = Vec::new();
    lex(text, &mut tokens);

    let mut result = String::new();
    pigify(&tokens, &mut result);

    println!("{result}");
}