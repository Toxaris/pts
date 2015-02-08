{-# LANGUAGE FlexibleInstances #-}
module PTS.Dynamics.Value
  ( Value
    ( Function
    , Number
    , Constant
    , PiType
    , ResidualIntOp
    , ResidualIfZero
    , ResidualVar
    , ResidualApp
    )
  , Function
  , abstract
  , open
  ) where

import PTS.Syntax.Constants (C)
import PTS.Syntax.Names (Name)
import PTS.Syntax.Term (BinOp)

newtype Function m = MkFunction (Value m -> m (Value m))

open :: Function m -> Value m -> m (Value m)
open (MkFunction f) v = f v

abstract :: Monad m => (Value m -> m (Value m)) -> m (Function m)
abstract f = return (MkFunction f)

instance Show (Function m) where
  show t = "<function>"

data Value m
  = Function  Name (Value m) (Function m)
  | Number    Integer
  | Constant  C
  | PiType    Name (Value m) (Function m) C
  | ResidualIntOp  BinOp (Value m) (Value m)
  | ResidualIfZero (Value m) (Value m) (Value m)
  | ResidualVar    Name
  | ResidualApp    (Value m) (Value m)
--  deriving Show
