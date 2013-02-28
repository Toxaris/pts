module PTS.Value where

import PTS.AST (Name, BinOp)
import PTS.Constants (C)

data Value m
  = Function  Name (Value m) (Value m -> m (Value m))
  | Number    Integer
  | Constant  C
  | PiType    Name (Value m) (Value m -> m (Value m))
  | ResidualNatOp  Name BinOp (Value m) (Value m)
  | ResidualIfZero (Value m) (Value m) (Value m)
  | ResidualVar    Name
  | ResidualApp    (Value m) (Value m)
