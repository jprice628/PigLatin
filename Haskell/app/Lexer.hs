-- Provides a function the tokenizes a string.
module Lexer (lex') where

import Tokens ( Token(..) )

-- A function that tokenizes a string. Note: tokens are returned in reverse
-- order.
lex' :: String -> [Token]
lex' text =
  let state0 = State [] []
      state1 = finalize $ foldl push state0 text
   in getTokens state1

-- Pushes a character onto a state, processing them and generating tokens as
-- appropriate.
push :: State -> Char -> State
push (State [] tokens) c
  | isAlpha c = State [c] tokens
  | otherwise = State [] (Special c : tokens)
push (State buffer tokens) c
  | isAlpha c = State (c : buffer) tokens
  | otherwise =
      let word = reverse buffer
       in State [] (Special c : Word word : tokens)

-- Converts any left over data in a states buffer into a token.
finalize :: State -> State
finalize s@(State buffer tokens)
  | null buffer = s
  | otherwise =
      let word = reverse buffer
       in State [] (Word word : tokens)

-- Gets the list of tokens in a state value.
getTokens :: State -> [Token]
getTokens (State _ tokens) = tokens

-- The list of word/non-special characters.
alpha :: String
alpha = "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz'"

-- Returns true is a character is a word character.
isAlpha :: Char -> Bool
isAlpha c = c `elem` alpha

-- Holds the state of a lexigraphical operation. The String value provides a
-- place to buffer characters. When appropriate, this buffer is converted and
-- pushed onto the list of tokens.
data State = State String [Token]
    deriving(Show)
