module Cipher where

import Data.Char

messageLength :: String -> Int
messageLength msg =
  foldr (\x y -> length x + y) 0 (words msg)

repeatKeyword :: String -> String -> String
repeatKeyword msg keyword =
  take (messageLength msg) $ concat $ repeat keyword

charShift :: Char -> Int
charShift c = (ord c) - (ord 'A')

main = do
  let msg = "MEET AT DAWN"
  let cipher = "ALLY"
  let shifts = map charShift $ repeatKeyword msg cipher
  let msgCharNums = (concat $ map (\x -> map ord x) $ words msg)
  zipWith (+) shifts msgCharNums
