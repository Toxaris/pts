{-# LANGUAGE FlexibleInstances #-}
module PTS.AST.Arbitrary where

import Test.QuickCheck

import PTS.AST
import PTS.Instances
import PTS.Instances.Arbitrary

instance Arbitrary Name where
  arbitrary = do
    growingElements (map read ["a", "b", "c", "d", "e", "f", "g", "h"])

instance Arbitrary Term where
  arbitrary = arbitraryTerm

class DistributeSize t where
  distributeSize :: Gen t

instance DistributeSize Int where
  distributeSize = sized (\size -> do
    return size)

instance DistributeSize (Int, Int) where
  distributeSize = sized (\size ->
    if size > 1 then do
      size1 <- choose (1, size - 1)
      return (size1, size - size1)
    else do
      fail "not enough size to distribute over two subterms.")

instance DistributeSize (Int, Int, Int) where
  distributeSize = sized (\size ->
    if size > 2 then do
      size1 <- choose (1, size - 2)
      size2 <- choose (1, size - size1 - 1)
      return (size1, size2, size - size1 - size2)
    else do
      fail "not enough size to distribute over three subterms.")

arbitraryTerm = sized (\size -> oneof $
    [  arbitraryNat
    ,  arbitraryVar
    ,  arbitraryPos
    ,  arbitraryConst
    ] ++
    (if size >= 2 then
    [  arbitraryApp
    ,  arbitraryNatOp
    ] else []) ++
    (if size >= 3 then
    [  arbitraryIfZero
    ,  arbitraryLam
    ,  arbitraryPi
    ] else []))

arbitraryNat = do
  value <- arbitrary
  return (mkNat value)

arbitraryNatOp = do
  (s1, s2) <- distributeSize

  t1      <-  resize s1 arbitrary
  t2      <-  resize s2 arbitrary
  (n, f)  <-  elements
                [  (read "add", Add)
                ,  (read "sub", Sub)
                ,  (read "mul", Mul)
                ,  (read "div", Div)
                ]
  return (mkNatOp n f t1 t2)

arbitraryIfZero = do
  (s1, s2, s3) <- distributeSize

  t1  <-  resize s1 arbitrary
  t2  <-  resize s2 arbitrary
  t3  <-  resize s3 arbitrary
  return (mkIfZero t1 t2 t3)

arbitraryVar = do
  n  <-  arbitrary
  return (mkVar n)

arbitraryConst = do
  c  <-  arbitrary
  return (mkConst c)

arbitraryApp = do
  (s1, s2) <- distributeSize
  t1  <-  resize s1 arbitrary
  t2  <-  resize s2 arbitrary
  return (mkApp t1 t2)

arbitraryLam = do
  (s, s1, s2) <- distributeSize

  n   <-  resize s arbitrary
  t1  <-  resize s1 arbitrary
  t2  <-  resize s2 arbitrary
  return (mkLam n t1 t2)

arbitraryPi = do
  (s, s1, s2) <- distributeSize

  n   <-  resize s arbitrary
  t1  <-  resize s1 arbitrary
  t2  <-  resize s2 arbitrary
  return (mkPi n t1 t2)

arbitraryPos = do
  t <- arbitrary
  return t
