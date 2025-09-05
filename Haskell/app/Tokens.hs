-- Provides tokens for use as output from lexigraphical operations and functions
-- for printing them.
module Tokens
  ( Token (..),
    printTokens,
    printToken,
  )
where

-- Tokens for use as output from lexigraphical operations.
data Token
  -- Represents a word, a small string of alphabetical characters.
  = Word String

  -- Represents a special character, like spaces and punctionation marks.
  | Special Char
  deriving(Show)

-- Prints a list of tokens to the console.
printTokens :: [Token] -> IO ()
printTokens = mapM_ printToken

-- Prints a single token to the console.
printToken :: Token -> IO ()
printToken (Word text) =
  let formattedString = "Word: " ++ text
   in putStrLn formattedString
printToken (Special c) =
  let formattedString = "Special: " ++ show c
   in putStrLn formattedString