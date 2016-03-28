{-# LANGUAGE FlexibleInstances #-}

import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)

-- Semigroup
-- class Semigroup a where
--   (<>) :: a -> a -> a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

-- conj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj False <> _ = BoolConj False
  _ <> BoolConj False = BoolConj False
  _ <> _ = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> choose (False,True)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- disj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> BoolDisj True = BoolDisj True
  _ <> _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> choose (False, True)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- or
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  Fst a <> Snd b = Snd b
  Fst a <> Fst b = Fst b
  Snd a <> Fst b = Snd a
  Snd a <> Snd b = Snd a

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

-- combine
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (\x -> f x <> g x)

-- comp
newtype Comp a = Comp { unComp :: (a -> a) }

instance (Semigroup a) => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

-- validation
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) = undefined

-- accumulate right
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Failure a) <> AccumulateRight (Success b) = AccumulateRight (Success b)
  AccumulateRight (Success a) <> AccumulateRight (Failure b) = AccumulateRight (Success a)
  AccumulateRight (Success a) <> AccumulateRight (Success b) = AccumulateRight (Success (a <> b))

-- main
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc Trivial)
  quickCheck (semigroupAssoc :: TwoAssoc Trivial Trivial)
  quickCheck (semigroupAssoc :: ThreeAssoc Trivial Trivial Trivial)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
