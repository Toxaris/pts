module PTS.Dynamics.Value where

import PTS.Syntax (Name, BinOp, C)

data Value m
  = Function  Name (Value m) (Value m -> m (Value m))
  | Number    Integer
  | Constant  C
  | PiType    Name (Value m) (Value m -> m (Value m))
  | ResidualIntOp  Name BinOp (Value m) (Value m)
  | ResidualIfZero (Value m) (Value m) (Value m)
  | ResidualVar    Name
  | ResidualApp    (Value m) (Value m)
