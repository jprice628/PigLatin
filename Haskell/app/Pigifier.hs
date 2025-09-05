module Pigifier (pigify) where

import Tokens (Token (..))
import Lexer (lex')

pigify :: String -> String
pigify [] = []
pigify input =
    let tokens = lex' input
     in foldl pigifyToken "" tokens

pigifyToken :: String -> Token -> String
pigifyToken result (Special c) = c : result
pigifyToken result (Word []) = result
pigifyToken result (Word text@(c : _))
  | isVowel c = text ++ "-yay" ++ result
  | otherwise =
      let pigified = buildResult $ foldl pushChar StartState text
       in pigified ++ result

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

vowels :: String
vowels = "AaEeIiOoUuYy"

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

isQ :: Char -> Bool
isQ c = c == 'Q' || c == 'q'

isU :: Char -> Bool
isU c = c == 'U' || c == 'u'

data State
    = StartState
    | QFirst Char
    | QU Char Char
    | Consonant String
    | SingleC Char
    | Normal String String