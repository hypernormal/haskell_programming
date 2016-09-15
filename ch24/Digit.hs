module Digit where

import Text.Trifecta
import Data.Char

parseDigit :: Parser Char
parseDigit = oneOf "1234567890"

base10Integer :: Parser Integer
base10Integer = do
  digits <- some parseDigit
  return $ toInteger $ sum $ zipWith (*) (iterate (10*) 1) (map digitToInt $ reverse digits)

base10Integer' :: Parser Integer
base10Integer' = do
  negative <- option ' ' (char '-')
  digits <- base10Integer
  case negative of
    '-' -> return $ negate digits
    _ -> return $ digits
