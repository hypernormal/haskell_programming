module Ch4Exercises where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =
  x == reverse x

myAbs :: Integer -> Integer
myAbs x =
  if x < 0 then 0 - x else x

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f (a,b) (c,d) = ((b,d), (a,c))

x = (+)

add1 xs =
  w `x` 1
    where w = length xs
