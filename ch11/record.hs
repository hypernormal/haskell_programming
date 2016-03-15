module Jammin where

import Data.List

data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

data Fruit =
  Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Ord, Show)

data JamJars =
  Jam { fruit :: Fruit
      , num :: Int }
      deriving (Eq, Ord, Show)

row1 = Jam Peach 5
row2 = Jam Plum 4
row3 = Jam Apple 7
row4 = Jam Blackberry 1
allJam = [row1, row2, row3, row4]

sumJam = sum $ map num allJam

compareKind (Jam _ k) (Jam _ k') = compare k k'
mostRow = head $ reverse $ sortBy compareKind allJam

groupJam = groupBy (\x y -> (fruit x) == (fruit y)) allJam
