dogYrs :: (Num a, Ord a) => a -> a
dogYrs x
  | x <= 0    = 0
  | x <= 1    = x * 15
  | x <= 2    = x * 12
  | x <= 4    = x * 8
  | otherwise = x * 6

pal xs
  | xs == reverse xs = True
  | otherwise        = False

numbers :: (Num a, Ord a) => a -> Int
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1
