module WordNumber where

import Data.List (intersperse)

wordNumber :: Int -> String
wordNumber =
  concat . intersperse "-" . map digitToWord . digits

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord n =
  ["one","two","three","four","five","six","seven","eight","nine"] !! (n-1)
