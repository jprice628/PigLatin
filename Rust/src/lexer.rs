use crate::tokens::Token;

// Used to determine whether a character is a word character.
const ALPHA: &str = "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz'";

// Scans an input string slice, creating tokens and placing them into a result
// vector.
pub fn lex<'a, 'b>(input: &'a str, result: &'b mut Vec<Token<'a>>) {
    if input.len() == 0 {
        panic!("Input should contain at least one character.");
    }

    if !input.is_ascii() {
        panic!("Input should contain only ASCII characters.");
    }

    let mut lexer: Lexer<'a, 'b> = Lexer::new(input, result);
    lexer.invoke();
}

// Manages state and performs lexigraphical analysis for the lex function.
struct Lexer<'a, 'b> {
    // The string slice that will be tokenized.
    input: &'a str,

    // The vector into which tokens will be placed.
    result: &'b mut Vec<Token<'a>>,

    // A buffer used to track slices of word characters.
    buffer: Option<(usize, usize)>
}

impl<'a, 'b> Lexer<'a, 'b> {
    // Instantiates a new lexer.
    fn new(input: &'a str, result: &'b mut Vec<Token<'a>>) -> Lexer<'a, 'b> {
        Lexer {
            input: input,
            result: result,
            buffer: None
        }
    }

    // Invokes the lexer.
    fn invoke(&mut self) {
        self.result.clear();

        for (i, c) in self.input.chars().enumerate() {
            if ALPHA.contains(c) {
                self.push_alpha(i);
            } else {
                self.push_special(i);
            }
        }

        // If the input string ends in one or more word characters, the buffer
        // will still be referencing some data.
        self.push_buffer();
    }

    // Pushes the index of an alpha character onto the buffer.
    fn push_alpha(&mut self, index: usize) {
        if let Some((min, _)) = self.buffer {
            self.buffer = Some((min, index + 1));
        } else {
            self.buffer = Some((index, index + 1));
        }
    }

    // Converts the index of a special character to a token and pushes it onto
    // the result vector.
    fn push_special(&mut self, index: usize) {
        // A special character is about to be added. If there is a word in the
        // buffer, push it first.
        self.push_buffer();

        let token: Token<'a> = Token::Special(&self.input[index..index + 1]);
        self.result.push(token);
    }

    // Pushes the contents of the buffer, if present, to the result vector and
    // resets the buffer.
    fn push_buffer(&mut self) {
        let Option::Some((start, end)) = self.buffer else {
            // The buffer is empty, so exit.
            return;
        };

        let token: Token<'a> = Token::Word(&self.input[start..end]);
        self.result.push(token);

        self.buffer = None;
    }
}