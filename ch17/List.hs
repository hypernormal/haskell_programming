import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- LIST
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid a => Monoid (List a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized genList

genList size
  | size > 0 = do v <- arbitrary
                  c <- genList (size `div` 2)
                  return (Cons v c)
  | otherwise = return Nil

instance Eq a => EqProp (List a) where (=-=) = eq

-- ZIPLIST
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (pure a)
  (ZipList' a) <*> (ZipList' b) = ZipList' (a <*> b)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    x <- arbitrary
    return $ ZipList' $ concat' x

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

-- FUNCTIONS
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap f as = concat' $ fmap f as

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' i Nil = Nil
take' i (Cons x xs) = Cons x (take' (i-1) xs)

-- sum
instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary
