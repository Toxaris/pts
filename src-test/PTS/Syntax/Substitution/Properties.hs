module PTS.Syntax.Substitution.Properties where

import Data.Set

import Test.Property

import PTS.Syntax
import PTS.Syntax.Arbitrary

substAvoidsCapture t x t' =
  x `member` freevars t ==>
  delete x (freevars t) `union` freevars t' == freevars (subst t x t')
