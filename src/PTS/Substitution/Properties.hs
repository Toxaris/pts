{-# LANGUAGE ScopedTypeVariables #-}
module PTS.Substitution.Properties where

import Data.Set

import Test.QuickCheck hiding ((.&.))
import Test.Framework.Providers.QuickCheck2 (testProperty)

import PTS.AST
import PTS.AST.Arbitrary
import PTS.Substitution

import PTS.Pretty

substAvoidsCapture = testProperty "substitution avoids capture" $
  \t x t' ->
  x `member` freevars t ==>
  delete x (freevars t) `union` freevars t' == freevars (subst t x t')
