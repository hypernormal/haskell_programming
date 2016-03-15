cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"

frappe = flippy "haha"

sum' :: (Eq a, Num a) => a -> a
sum' 1 = 1
sum' n = n + sum' (n-1)

mult' :: (Eq a, Num a) => a -> a -> a
mult' x 0 = 0
mult' x y = x + mult' x (y-1)

mc91 n
  | n > 100  = n - 10
  | n <= 100 = mc91(mc91(n + 11))

