module Tokens
  ( Token (..),
    printTokens,
    printToken,
  )
where

data Token
  = Word String
  | Special Char
  deriving(Show)

printTokens :: [Token] -> IO ()
printTokens = mapM_ printToken

printToken :: Token -> IO ()
printToken (Word text) =
  let formattedString = "Word: " ++ text
   in putStrLn formattedString
printToken (Special c) =
  let formattedString = "Special: " ++ show c
   in putStrLn formattedString