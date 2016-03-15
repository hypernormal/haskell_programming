div' :: Integral a => a -> a -> (a, a)
div' num denom =
  go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)
