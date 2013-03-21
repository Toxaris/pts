module PTS.Syntax.Names
  ( Name
  , Names
  , freshvarl
  , ModuleName (ModuleName)
  , parts
  ) where

import Parametric.AST (Name, Names, freshvarl)

data ModuleName
  =  ModuleName [String]
  deriving (Eq)

parts :: ModuleName -> [String]
parts (ModuleName xs) = xs
