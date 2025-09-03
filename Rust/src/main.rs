mod tokens;

use crate::tokens::Token;

fn main() {
    let word = Token::Word("word");
    println!("{word}");

    let special = Token::Special("!");
    println!("{special}");
}
