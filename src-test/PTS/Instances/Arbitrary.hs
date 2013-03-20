module PTS.Instances.Arbitrary where

import Test.QuickCheck

import PTS.Syntax

instance Arbitrary C where
  arbitrary = sized (\size -> do
    number <- choose (0, 2 + (size `div` 20))
    return (C number))
