data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in
    if n == "Bess" && w > 499 then Nothing else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just age ->
            weightCheck (Cow nammy age weight')

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  name <- noEmpty name'
  age <- noNegative age'
  weight <- noNegative weight'
  weightCheck (Cow name age weight)

f :: Maybe Integer
f = Just 1

g :: Maybe String
g = Just "1"

h :: Maybe Integer
h = Just 10191

zed :: a -> b -> c -> (a, b, c)
zed = (,,)

doSomething = do
  a <- f
  b <- g
  c <- h
  return (zed a b c)

zed' :: Monad m => a -> b -> c -> m (a, b, c)
zed' a b c = return (a, b, c)

doSomething' = do
  a <- f
  b <- g
  c <- h
  zed' a b c
