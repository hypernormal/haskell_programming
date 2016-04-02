import Control.Monad
import Control.Applicative
import Data.Monoid
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation err a =
    Failure err
  | Success a
  deriving (Eq, Show)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data Errors = DividedByZero | StackOverflow | Moogles deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure e <*> Failure e' = Failure (e `mappend` e')
  Failure e <*> Success _ = Failure e
  Success f <*> a = fmap f a

instance Arbitrary Errors where
  arbitrary = elements [DividedByZero, StackOverflow, Moogles]

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [liftM Failure arbitrary, liftM Success arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

main = do
  quickBatch $ applicative (undefined :: Validation [Errors] (Integer, Integer, Integer))
