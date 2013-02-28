{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Tools.Errors.Class
  ( MonadErrors (..)
  , module Control.Monad.Error.Class
  ) where

import Control.Monad.Error.Class
import Control.Monad.Writer

import Data.Monoid

class MonadError e m => MonadErrors e m | m -> e where
  recover :: a -> m a -> m a
  annotate :: (e -> e) -> m a -> m a

instance (Monoid w, MonadErrors e m) => MonadErrors e (WriterT w m) where
  recover x p = WriterT (recover (x, mempty) (runWriterT p))
  annotate f p = WriterT (annotate f (runWriterT p))
