module PTS.Instances.Arbitrary where

import Test.QuickCheck

import PTS.Instances
import PTS.Constants

instance Arbitrary C where
  arbitrary = sized (\size -> do
    number <- choose (0, size - 1)
    return (C number))
