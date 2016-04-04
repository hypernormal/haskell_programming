import Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a
