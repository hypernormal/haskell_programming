module Ch5Currying where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

funcIgnoresArgs :: a -> a -> a -> String
funcIgnoresArgs x y z = "Blah"

h :: (Num a, Num b) => a -> b -> b
h x y = y

jackal :: (Ord a, Eq b) => a -> b -> a
jackal x y = x

