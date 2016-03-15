mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

myMap _ [] = []
myMap f (x:xs) = f x : map f xs
