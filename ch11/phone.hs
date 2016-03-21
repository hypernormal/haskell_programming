import Data.Char
import Data.List
import Data.Function
import Data.Ord

type Symbol = Char
type Symbols = [Symbol]
type Digit = Char
type Presses = Int
data DaPhone = Phone [(Digit, Symbols)] deriving Show

phone = Phone [
        ('1', "1")
      , ('2', "abc2")
      , ('3', "def3")
      , ('4', "ghi4")
      , ('5', "jkl5")
      , ('6', "mno6")
      , ('7', "pqrs7")
      , ('8', "tuv8")
      , ('9', "wxyz9")
      , ('*', "^")
      , ('0', "_0")
      , ('#', ".,")
      ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever blah",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u kno da truth"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c
  | isUpper c = [capital, (digit, presses)]
  | otherwise = [(digit, presses)]
  where
    (digit, symbols) = findMapping phone (toLower c)
    presses = findPresses c symbols

capital :: (Digit, Presses)
capital = ('*', 1)

space :: (Digit, Symbols)
space = ('0', "_")

findMapping :: DaPhone -> Char -> (Digit, Symbols)
findMapping _ ' ' = space
findMapping (Phone mappings) c =
  head $ filter (\(d, s) -> c `elem` s) mappings

findPresses :: Char -> Symbols -> Presses
findPresses c s =
  case elemIndex c s of
    Just i -> i + 1
    Nothing -> 1

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone s = concat $ map (reverseTaps phone) s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_,p) y -> p + y) 0

mostPopularLetter :: String -> Char
mostPopularLetter s = c
  where
    taps = cellPhonesDead phone s
    mostTaps = last
          $ sortBy (\x y -> compare (length x) (length y))
          $ groupBy (==)
          $ sortBy (comparing fst) taps
    c = tapsToChar phone mostTaps

tapsToChar :: DaPhone -> [(Digit, Presses)] -> Char
tapsToChar (Phone mappings) ((d, p):xs) = c
  where
    (digit, symbols) = head $ filter (\(a,b) -> a == d) mappings
    c = symbols !! (p - 1)

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined
