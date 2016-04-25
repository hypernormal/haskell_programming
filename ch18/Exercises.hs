import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Control.Monad

-- NOPE
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  _ <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Eq a) => EqProp (Nope a) where (=-=) = eq

-- LIST

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  Cons f fs <*> xs = append (fmap f xs) (fs <*> xs)

instance Monad List where
  return = pure
  -- (>>=) :: List a -> (a -> List b) -> List b
  Nil >>= _ = Nil
  Cons x xs >>= f = append (f x) (xs >>= f)

-- Other

j :: Monad m => m (m a) -> m a
j xs = xs >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f xs = fmap f xs

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f xs ys = xs >>= \x -> ys >>= \y -> return (f x y)

l3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
l3 f xs ys zs = xs >>= \x -> ys >>= \y -> zs >>= \z -> return (f x y z)

a :: Monad m => m a -> m (a -> b) -> m b
a m mf = mf >>= \f -> fmap f m

meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- [1,2,3] -> (\x -> Just (x+1)) -> Just [2,3,4]
meh xs f = sequence (fmap f xs)

main = do
  quickBatch (monad (NopeDotJpg :: Nope (Int, Int, Int)))
