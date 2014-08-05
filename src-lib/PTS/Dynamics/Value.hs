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
  , ValueFunction
    ( ValueFunction
    )
  , open
  ) where

import PTS.Syntax.Constants (C)
import PTS.Syntax.Names (Name)
import PTS.Syntax.Term (BinOp)

newtype ValueFunction m = ValueFunction (Value m -> m (Value m))

open :: ValueFunction m -> Value m -> m (Value m)
open (ValueFunction f) v = f v

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
