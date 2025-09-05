module Main where

import Tokens

main :: IO ()
main = 
    let tokens = [Word "Hello", Special ',', Special ' ', Word "World", Special '!']
     in printTokens tokens
