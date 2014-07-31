{-# LANGUAGE FlexibleInstances #-}
module PTS.Dynamics.Value where

import PTS.Syntax.Constants (C)
import PTS.Syntax.Names (Name)
import PTS.Syntax.Term (BinOp)

data ValueFunction m = ValueFunction (Value m -> m (Value m))

callFunction :: ValueFunction m -> Value m -> m (Value m)
callFunction (ValueFunction f) v = f v

instance Show (ValueFunction m) where
  show t = "<function>"

data Value m
  = Function  Name (Value m) (ValueFunction m)
  | Number    Integer
  | Constant  C
  | PiType    Name (Value m) (ValueFunction m) C
  | ResidualIntOp  BinOp (Value m) (Value m)
  | ResidualIfZero (Value m) (Value m) (Value m)
  | ResidualVar    Name
  | ResidualApp    (Value m) (Value m)
  deriving Show
