module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myBreak :: String -> Char -> [String]
myBreak str chr = go str chr []
      where
        go "" _ xs = xs
        go str chr xs =
          let str' = dropWhile (== chr) str in
              go (dropWhile (/= chr) str') chr (xs ++ [takeWhile (/= chr) str'])

myLines :: String -> [String]
myLines str = myBreak str '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $ "Are they equal? "
          ++ show (myLines sentences == shouldEqual)
