twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  xs >>= (\x -> if even x then [x*x, x*x] else [])
