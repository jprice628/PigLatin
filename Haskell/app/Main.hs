module Main where

import Tokens ( printTokens )
import Lexer ( lex' )

main :: IO ()
main =
    let input = "The quick brown fox jumped over the lazy sleeping dog."
        tokens = lex' input
     in printTokens tokens
