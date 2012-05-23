{-# LANGUAGE TypeSynonymInstances #-}
module Tools.Instances where

import Prelude ()

import Control.Applicative(Applicative(..), Alternative(..))
import Control.Monad(Monad(..), MonadPlus(..), ap)
import Control.Monad.Error(Error, ErrorT)
import Control.Monad.Identity(Identity)
import Control.Monad.Reader
import Control.Monad.Writer(Writer, WriterT)

import Data.Monoid(Monoid)

import Text.ParserCombinators.Parsec(GenParser)

-- instance Applicative (GenParser a st) where
--   pure = return
--   (<*>) = ap
  
-- instance Alternative (GenParser a st) where
--   empty = mzero
--   (<|>) = mplus

-- instance Monad m => Applicative (ReaderT r m) where
--   pure = return
--   (<*>) = ap
  
-- instance (Applicative m, MonadPlus m) => Alternative (ReaderT r m) where
--   empty = mzero
--   (<|>) = mplus
-- 
-- instance (Error e, Monad m) => Applicative (ErrorT e m) where
--   pure = return
--   (<*>) = ap
--   
-- instance (Monad m, Error e) => Alternative (ErrorT e m) where
--   empty = mzero
--   (<|>) = mplus
-- 
-- instance (Monoid w, Monad m) => Applicative (WriterT w m) where
--   pure = return
--   (<*>) = ap
