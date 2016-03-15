import Data.Char

filterUpper :: [Char] -> [Char]
filterUpper = filter isUpper

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

capitalizeAll :: String -> String
capitalizeAll xs = map toUpper xs

capitalizeFirst :: String -> Char
capitalizeFirst = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x == True then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem ele (x:xs) = if ele == x then True else myElem ele xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' ele xs = myAny (\x -> x == ele) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish xs = foldl (++) [] xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldl (++) [] $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\x -> x) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x
  where
    go f [] max = max
    go f (x:xs) max =
      case f max x of
        GT -> go f (xs) max
        LT -> go f (xs) x
        EQ -> go f (xs) max

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f xs x
  where
    go f [] min = min
    go f (x:xs) min =
      case f min x of
        GT -> go f (xs) x
        LT -> go f (xs) min
        EQ -> go f (xs) min

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
