import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

-- pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

propPairId :: Pair [Int] -> Bool
propPairId x = functorIdentity x

propPairComp :: Pair [Int] -> Bool
propPairComp x = functorCompose (++ [1]) (reverse) x

-- two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

propTwoId :: Two Bool Int -> Bool
propTwoId x = functorIdentity x

propTwoComp :: Two Bool Int -> Bool
propTwoComp x = functorCompose (*3) (+2) x

-- three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

propThreeId :: Three Bool Char Int -> Bool
propThreeId x = functorIdentity x

propThreeComp :: Three Bool Char Int -> Bool
propThreeComp x = functorCompose (*3) (+2) x

main :: IO ()
main = do
  quickCheck propPairId
  quickCheck propPairComp
  quickCheck propTwoId
  quickCheck propTwoComp
  quickCheck propThreeId
  quickCheck propThreeComp
