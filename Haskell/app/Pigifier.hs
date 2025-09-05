-- Provides a function that translates a string to ig-Pay atin-Lay.
module Pigifier (pigify) where

import Tokens (Token (..))
import Lexer (lex')
import Data.List (foldl')

-- A function that translates a string to ig-Pay atin-Lay.
pigify :: String -> String
pigify [] = []
pigify input =
    -- Note: lex' returns tokens in reverse order, which is perfect for building
    -- a new string from them using the cons operator.
    let tokens = lex' input
     in foldl' pigifyToken "" tokens

-- Translates the token and prepends it to the provided string.
pigifyToken :: String -> Token -> String
pigifyToken result (Special c) = c : result
pigifyToken result (Word []) = result
pigifyToken result (Word text@(c : _))
  | isVowel c = text ++ "-yay" ++ result
  | otherwise =
      let pigified = buildResult $ foldl' pushChar StartState text
       in pigified ++ result

-- Pushes a character onto a state.
pushChar :: State -> Char -> State
pushChar StartState c
  | isQ c = QFirst c
  | otherwise = SingleC c
pushChar (QFirst q) c
  | isU c = QU q c
  | isVowel c = Normal [q] [c]
  | otherwise = Consonant (c : [q])
pushChar (QU q u) c = Normal (u : [q]) [c]
pushChar (Consonant conCluster) c
  | isVowel c = Normal conCluster [c]
  | otherwise = Consonant (c : conCluster)
pushChar (SingleC s) c
  | isVowel c = Normal [s] [c]
  | otherwise = Consonant (c : [s])
pushChar (Normal conCluster end) c = Normal conCluster (c : end)

-- Builds a pig latin word from a state.
buildResult :: State -> String
buildResult StartState = ""
buildResult (QFirst q) = [q]
buildResult (QU q u) = [u] ++ "-" ++ [q] ++ "ay"
buildResult (Consonant conCluster) = reverse conCluster
buildResult (SingleC s) = [s]
buildResult (Normal conCluster end) =
  let a = reverse end
      b = reverse conCluster
   in a ++ "-" ++ b ++ "ay"

-- The list of vowels.
vowels :: String
vowels = "AaEeIiOoUuYy"

-- Returns true if the specified character is a vowel.
isVowel :: Char -> Bool
isVowel c = c `elem` vowels

-- Returns true if the specified character is a Q.
isQ :: Char -> Bool
isQ c = c == 'Q' || c == 'q'

-- Returns true if the specified character is a U.
isU :: Char -> Bool
isU c = c == 'U' || c == 'u'

-- A state type used to translate words into Pig Latin.
data State
    = StartState
    | QFirst Char
    | QU Char Char
    | Consonant String
    | SingleC Char
    | Normal String String