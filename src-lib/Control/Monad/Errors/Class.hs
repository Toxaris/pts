{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Control.Monad.Errors.Class
  ( MonadErrors (..)
  , module Control.Monad.Except
  ) where

import Control.Monad.Except
import Data.Monoid
import Control.Monad.Writer(WriterT, mapWriterT)
import Control.Monad.Reader(ReaderT, mapReaderT)
import Control.Monad.State (StateT (StateT), runStateT, mapStateT)

class MonadError e m => MonadErrors e m | m -> e where
  recover :: a -> m a -> m a
  annotate :: (e -> e) -> m a -> m a


instance (Monoid e, MonadErrors e m) => MonadErrors e (ReaderT r m) where
  annotate f = mapReaderT (annotate f)
  recover x = mapReaderT (recover x)

instance (Monoid e, MonadErrors e m) => MonadErrors e (StateT s m) where
  annotate f = mapStateT (annotate f)
  recover x p = StateT (\s -> recover (x, s) (runStateT p s))

instance (Monoid w, MonadErrors e m) => MonadErrors e (WriterT w m) where
  recover x = mapWriterT (recover (x, mempty))
  annotate f = mapWriterT (annotate f)
