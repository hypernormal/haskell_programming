import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' y xs = result
  where
    Any result = foldMap (Any . (==y)) xs

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = x
  where
    Min x = foldMap (Min . Just) xs

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs = x
  where
    Max x = foldMap (Max . Just) xs

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ y -> y+1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (\x y -> x : y) []

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r
  

newtype Min a = Min { getMin :: Maybe a } deriving Show
instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  m `mappend` Min Nothing = m
  Min Nothing `mappend` n = n
  (Min m@(Just x)) `mappend` (Min n@(Just y))
    | x <= y = Min m
    | otherwise = Min n

newtype Max a = Max { getMax :: Maybe a } deriving Show
instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  m `mappend` Max Nothing = m
  Max Nothing `mappend` n = n
  (Max m@(Just x)) `mappend` (Max n@(Just y))
    | x >= y = Max m
    | otherwise = Max n
