module Digit where

import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf "1234567890"

base10Integer :: Parser Integer
base10Integer = fmap read (many parseDigit)
