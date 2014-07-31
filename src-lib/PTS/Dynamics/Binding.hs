{-# LANGUAGE FlexibleContexts #-}
module PTS.Dynamics.Binding
  (  Binding (..)
  ,  Bindings
  ,  lookupValue
  ,  lookupType
  )  where

import Control.Monad.Environment

import Prelude hiding (lookup)

import PTS.Dynamics.Value (Value)
import PTS.Syntax (Name, TypedTerm)

type Bindings m = [(Name, Binding m)]

data Binding m
  = Binding
    { bindingExport :: Bool
    , bindingValue :: Value m
    , bindingType :: TypedTerm
    }
  deriving Show

lookupValue :: MonadEnvironment Name (Binding n) m => Name -> m (Maybe (Value n))
lookupValue x = do
  m <- lookup x
  return (fmap bindingValue m)

lookupType :: MonadEnvironment Name (Binding n) m => Name -> m (Maybe TypedTerm)
lookupType x = do
  m <- lookup x
  return (fmap bindingType m)
