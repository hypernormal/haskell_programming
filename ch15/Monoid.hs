import Data.Monoid

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)
  x `mappend` y =
    Mem $ \s ->
      let
        (a, s') = runMem y s
        (a', s'') = runMem x s'
      in (a' `mappend` a, s'')
