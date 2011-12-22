module PTS.Instances.Arbitrary where

import Test.QuickCheck

import PTS.Instances

instance Arbitrary C where
  arbitrary = sized (\size -> do
    number <- choose (1, size - 1)
    return (C number))
