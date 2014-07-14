{-# LANGUAGE FlexibleInstances #-}
module PTS.Dynamics.Value where

import PTS.Syntax (Name, BinOp, C)

newtype ValueFunction m = ValueFunction {callFunction :: Value m -> m (Value m)}

instance Show (ValueFunction m) where
  show t = "<function>"

data Value m
  = Function  Name (Value m) (ValueFunction m)
  | Number    Integer
  | Constant  C
  | PiType    Name (Value m) (ValueFunction m)
  | ResidualIntOp  Name BinOp (Value m) (Value m)
  | ResidualIfZero (Value m) (Value m) (Value m)
  | ResidualVar    Name
  | ResidualApp    (Value m) (Value m)
  deriving Show
