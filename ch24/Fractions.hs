module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

parseDecimal :: Parser Rational
parseDecimal = do
  digits <- integer
  char '.'
  decimals <- integer
  return $ fromIntegral digits + toDecimal decimals

fracOrDecimal = try parseDecimal <|> parseFraction

toDecimal :: (Fractional a) => Integer -> a
toDecimal d = fromIntegral d / (10 ^^ (length $ show d))

parseInt :: Parser Integer
parseInt = do
  x <- integer
  eof
  return (x)
