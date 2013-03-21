module PTS.Syntax.Names
  ( Name
  , Names
  , ModuleName (ModuleName)
  , parts
  ) where

import Parametric.AST (Name, Names)

data ModuleName
  =  ModuleName [String]
  deriving (Eq)

parts :: ModuleName -> [String]
parts (ModuleName xs) = xs
