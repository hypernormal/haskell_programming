import Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

replaceThe :: String -> String
replaceThe s = unwords $ map (replaceHelper . notThe) $ words s

replaceHelper :: Maybe String -> String
replaceHelper (Just a) = a
replaceHelper Nothing = "a"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = countTheBeforeVowel' $ map notThe $ words s

countTheBeforeVowel' :: [Maybe String] -> Integer
countTheBeforeVowel' [] = 0
countTheBeforeVowel' (Nothing:(Just a):xs) =
  if vowelWord a
  then 1 + countTheBeforeVowel' (xs)
  else 0 + countTheBeforeVowel' (xs)
countTheBeforeVowel' (x:xs) = countTheBeforeVowel' xs

vowelWord (x:xs) = isVowel x

vowels = "aeiou"
isVowel c = c `elem` vowels

countVowels :: String -> Integer
countVowels s = toInteger $ length $ filter (\x -> x) $ map isVowel s

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if length x > length y then Just (Word' s) else Nothing
  where
    [x,y] = group $ sort $ map isVowel s

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just $ last $ take ((fromInteger i)+1) $ iterate Succ Zero

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a ma = mayybee a id ma

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just a):xs) = a : (catMaybes xs)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = xs'
  where
    filtered = filter (\x -> isJust x) xs
    xs' = if length filtered < length xs then Nothing else Just (catMaybes filtered)

isLeft (Left a) = True
isLeft (Right b) = False

isRight (Left a) = False
isRight (Right b) = True

lefts' :: [Either a b] -> [a]
lefts' xs = foldr (\(Left a) y -> a : y) [] (filter isLeft xs)

rights' :: [Either a b] -> [b]
rights' xs = foldr (\(Right b) y -> b : y) [] (filter isRight xs)

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (Just . f) x
