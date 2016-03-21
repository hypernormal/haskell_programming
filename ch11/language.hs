import Data.Char
import Data.List.Split
import Data.List

-- Language exercise

-- #1
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

-- #2
capitalizeParagraph :: String -> String
capitalizeParagraph =
  (intercalate ". " . map capitalizeWord . splitOn ". ")


