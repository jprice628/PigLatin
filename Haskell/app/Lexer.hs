module Lexer (lex') where

import Tokens ( Token(..) )

lex' :: String -> [Token]
lex' text =
  let state0 = State [] []
      state1 = finalize $ foldl push state0 text
   in getTokens state1

push :: State -> Char -> State
push (State [] tokens) c
  | isAlpha c = State [c] tokens
  | otherwise = State [] (Special c : tokens)
push (State buffer tokens) c
  | isAlpha c = State (c : buffer) tokens
  | otherwise =
      let word = reverse buffer
       in State [] (Special c : Word word : tokens)

finalize :: State -> State
finalize s@(State buffer tokens)
  | null buffer = s
  | otherwise =
      let word = reverse buffer
       in State [] (Word word : tokens)

getTokens :: State -> [Token]
getTokens (State _ tokens) = tokens

alpha :: String
alpha = "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz'"

isAlpha :: Char -> Bool
isAlpha c = c `elem` alpha

data State = State String [Token]
    deriving(Show)
