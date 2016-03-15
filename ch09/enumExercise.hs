myEnumFromTo :: (Enum a, Ord a) => a -> a -> [a]
myEnumFromTo from to =
  go from to []
    where go from' to' xs
           | from' > to' = xs
           | otherwise   = go (succ from') to' (xs ++ [from'])
