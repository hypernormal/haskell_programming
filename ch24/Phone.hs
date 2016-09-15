module Phone where

import Control.Applicative
import Text.Trifecta
import Data.Monoid
import Data.Char

type AreaCode = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber AreaCode Exchange LineNumber deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  option "" $ string "1-"
  area <- (count 3 digit) <|> (char '(' *> (count 3 digit) <* char ')')
  exchange <- (count 3 digit) <|> (char ' ' >> (count 3 digit)) <|> (char '-' >> (count 3 digit))
  line <- (count 4 digit) <|> (char '-' >> (count 4 digit))
  return $ PhoneNumber (read area :: Int) (read exchange :: Int) (read line :: Int)
