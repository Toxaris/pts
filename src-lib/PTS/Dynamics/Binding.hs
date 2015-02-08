{-# LANGUAGE FlexibleContexts #-}
module PTS.Dynamics.Binding
  (  Binding (..)
  ,  Bindings
  ,  lookupValue
  ,  lookupType
  )  where

import Control.Arrow ((&&&))
import Control.Monad.Environment

import Prelude hiding (lookup)

import PTS.Dynamics.Value (Value)
import PTS.Syntax (Name, TypedTerm, C)

type Bindings m = [(Name, Binding m)]

data Binding m
  = Binding
    { bindingExport :: Bool
    , bindingValue :: Value m
    , bindingType :: Value m
    , bindingSort :: Maybe C
    }
--  deriving Show

lookupValue :: MonadEnvironment Name (Binding n) m => Name -> m (Maybe (Value n))
lookupValue x = do
  m <- lookup x
  return (fmap bindingValue m)

lookupType :: MonadEnvironment Name (Binding n) m => Name -> m (Maybe (Value n, Maybe C))
lookupType x = do
  m <- lookup x
  return (fmap (bindingType &&& bindingSort) m)
