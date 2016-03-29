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

-- three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

propThree'Id :: Three' Bool Int -> Bool
propThree'Id x = functorIdentity x

propThree'Comp :: Three' Bool Int -> Bool
propThree'Comp x = functorCompose (*3) (*2) x

-- four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

propFourId :: Four Bool Int Char String -> Bool
propFourId x = functorIdentity x

propFourComp :: Four Bool Int Char String -> Bool
propFourComp x = functorCompose (++ " yo!") (head) x

-- four'
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

propFour'Id :: Four' Int String -> Bool
propFour'Id x = functorIdentity x

propFour'Comp :: Four' Int String -> Bool
propFour'Comp x = functorCompose (++ "world") (reverse) x

main :: IO ()
main = do
  quickCheck propPairId
  quickCheck propPairComp
  quickCheck propTwoId
  quickCheck propTwoComp
  quickCheck propThreeId
  quickCheck propThreeComp
  quickCheck propThree'Id
  quickCheck propThree'Comp
  quickCheck propFourId
  quickCheck propFourComp
  quickCheck propFour'Id
  quickCheck propFour'Comp
