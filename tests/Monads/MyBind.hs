module MyBind where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind fn ma = join $ fmap fn ma

