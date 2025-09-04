use crate::tokens::Token;
use crate::lexer::lex;

const VOWELS: &str = "AaEeIiOoUuYy";

pub fn pigify(input: &str) -> String {
    let tokens = lex(input);

    let mut result = String::new();
    let mut pigifier = Pigifier::new(&mut result);

    for token in tokens {
        pigifier.pigify_token(&token);
    }

    result
}

// A service used to convert words to pig latin.
struct Pigifier<'a> {    
    // Tracks the state of the service based on the characters that are
    // encountered.
    state: State,

    // Used to store the consonant cluster at the beginning of a word (if there
    // is one).
    c_cluster: String,

    // The result string to which the translated word will be appended.
    result: &'a mut String
}

impl<'a> Pigifier<'a> {
    // Creates a new pigifier.
    fn new(result: &'a mut String) -> Pigifier<'a> {
        Pigifier { 
            state: State::Start, 
            c_cluster: String::new(),
            result: result
        }
    }

    // Translates a token to Pig Latin and appends it to the result.
    fn pigify_token(&mut self, token: &Token<'a>) {
        match token {
            Token::Word(word) => self.push_word(word),
            Token::Special(special) => self.result.push_str(special)
        }
    }

    // Translates a word to Pig Latin and appends it to the result.
    fn push_word(&mut self, word: &str) {
        if let Some(c) = Self::single_consonant(word) {
            self.result.push(c);
        } else {
            for c in word.chars() {
                self.push_char(c);
            }

            self.finalize();
        }
    }

    fn single_consonant(word: &str) -> Option<char> {
        if word.len() != 1 { return None; } 
        
        let Some(c) = word.chars().next() else {
            panic!("word.chars().next() should return Some(c).");
        };

        if VOWELS.contains(c) { return None; }

        Some(c)
    }

    // Pushes a character to the pigifier for processing.
    fn push_char(&mut self, c: char) {
        match self.state {
            State::Start => self.push_start(c),
            State::Consonant => self.push_consonant(c),
            State::Q => self.push_q(c),
            State::Vowel => self.result.push(c)
        };
    }

    // Pushes a character to a pigifier in the start state.
    fn push_start(&mut self, c: char) {
        if c == 'Q' || c == 'q' {
            self.c_cluster.push(c);
            self.state = State::Q;
        } else if VOWELS.contains(c) {
            self.result.push(c);
            self.state = State::Vowel;
        } else {
            self.c_cluster.push(c);
            self.state = State::Consonant;
        }
    }

    // Pushes a character to a pigifier in the consonant state.
    fn push_consonant(&mut self, c: char) {
        if VOWELS.contains(c) {
            self.result.push(c);
            self.state = State::Vowel;
        } else {
            self.c_cluster.push(c);
        }
    }

    // Pushes a character to a pigifier in the q state.
    fn push_q(&mut self, c: char) {
        if c == 'U' || c == 'u' {
            self.c_cluster.push(c);
            self.state = State::Vowel;
        } else if VOWELS.contains(c) {
            self.result.push(c);
            self.state = State::Vowel;
        } else {
            self.c_cluster.push(c);
            self.state = State::Consonant;
        }
    }

    // Wraps up the operation according to the state and readies the service for
    // the next word.
    fn finalize(&mut self) {
        if self.c_cluster.len() > 0 {
            let temp = format!("-{}ay", self.c_cluster);
            self.result.push_str(&temp);

            self.c_cluster.clear();
        }
        else {
            self.result.push_str("-yay")
        }

        self.state = State::Start;
    }
}

// Provides state values for the Pigifier service.
enum State {
    // The entry state.
    Start,

    // The state when at least one character has been pushed and all pushed
    // characters are consonants.
    Consonant,

    // The state when the first and only letter pushed is Q or q.
    Q,

    // The state once a vowel has been encountered.
    Vowel,
}