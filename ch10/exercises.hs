stops = "pbtdkg"
vowels = "aeiou"

stopVowel = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["car", "dog", "ball"]
verbs = ["run", "walk", "throw"]

nounVerbNoun = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc :: (Fractional a) => String -> a
seekritFunc x =
    (/) (fromIntegral $ sum (map length (words x)))
        (fromIntegral $ length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x y -> f x || y) False xs

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = myAny (\y -> y == x) xs

myReverse :: [a] -> [a]
myReverse = foldr (\x y -> y ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x y -> [f x] ++ y) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x y -> if f x then x : y else y) [] xs

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldr (\x y -> f x ++ y) [] xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if f x y == GT then x else y) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> if f x y == LT then y else x) (head xs) xs

f x y z = h (subFunction x y z)
  where subFunction x y z = g x y z


