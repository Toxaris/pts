{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, FunctionalDependencies #-}
module Tools.Errors.Class
  ( MonadErrors (..)
  , module Control.Monad.Error.Class
  ) where

import Data.Monoid
import Control.Monad.Error.Class

class MonadError e m => MonadErrors e m | m -> e where
  recover :: a -> m a -> m a
  annotate :: (e -> e) -> m a -> m a
