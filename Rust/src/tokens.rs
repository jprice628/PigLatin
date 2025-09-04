use std::fmt::{Display, Formatter, Result};

// Used to represent lexigraphical units within a string of text.
pub enum Token<'a> {
    // Represents a word, e.g. apple, car, exit, etc.
    Word(&'a str),

    // Represents a special (non-word) character.
    Special(&'a str)
}

// Implements the Display trait for tokens to make them easier to log.
impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Token::Word(value) => write!(f, "Word({value})"),
            Token::Special(value) => write!(f, "Special({value})")
        }
    }
}