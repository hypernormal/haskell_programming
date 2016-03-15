module Print5Broken where

yar :: String
yar = "Yarrrrr"

printSecond :: IO ()
printSecond = do
  putStrLn yar

main :: IO ()
main = do
  putStrLn yar
  printSecond

