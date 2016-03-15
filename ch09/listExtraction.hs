myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n xs = go n xs []
     where
        go 0 xs rs     = rs
        go n (x:xs) rs = go (n-1) (xs) (rs ++ [x])

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x:xs) = myDrop (n-1) (xs)

myWords :: String -> [String]
myWords str = go str []
      where
        go "" xs = xs
        go str xs =
          let str' = dropWhile (== ' ') str in
          go (dropWhile (/= ' ') str') (xs ++ [takeWhile (/= ' ') str'])


