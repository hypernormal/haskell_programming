myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup a ((x, y):xs)
  | a == x = Just y
  | otherwise = myLookup a xs
