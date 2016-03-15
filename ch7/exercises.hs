f :: Ord a => a -> a -> Bool
f a b = True

tensDigit :: Integral a => a -> a
tensDigit x = d
  where d = mod (fst (divMod x 10)) 10

foldBool :: a -> a -> Bool -> a
foldBool x y z =
  case z of
    True -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
  | z = x
  | not z = y

g f (a, c) =
  ((f a), c)


