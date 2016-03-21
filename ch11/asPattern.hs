import Data.List (isInfixOf)
import Data.Char

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf sub@(x:xs) full =
  if isInfixOf [x] full then isSubsequenceOf xs full else False

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs =
  map (\word@(y:ys) -> (word, toUpper y : ys)) $ words xs
