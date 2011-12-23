{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, GADTs #-}
module Test.Property
  (  Property ()
  ,  property
  ,  (==>)
  ,  test
  ,  Q.Arbitrary ()
  )  where

import qualified Test.QuickCheck as Q

infixr 0 ==>

data Property a where
  BooleanProperty :: Bool -> Property Bool
  ConditionalProperty :: Bool -> Property a -> Property a
  UniversalProperty :: (Q.Arbitrary a, Show a) => (a -> Property b) -> Property (a -> b)

class Testable a b | a -> b where
  property :: a -> Property b

instance Testable Bool Bool where
  property = BooleanProperty

instance (Q.Arbitrary a, Show a, Testable b c) => Testable (a -> b) (a -> c) where
  property f = UniversalProperty (\x -> property (f x))

instance Testable (Property a) a where
  property = id

(==>) :: (Testable a b) => Bool -> a -> Property b
p ==> q = ConditionalProperty p (property q)

test :: Testable a b => a -> b
test p = testProperty (property p)

testProperty :: Property a -> a
testProperty (BooleanProperty p) = p
testProperty (ConditionalProperty p q)
  = if p then testProperty q else ignoreArguments q
testProperty (UniversalProperty f) = \x -> testProperty (f x)

ignoreArguments :: Property a -> a
ignoreArguments (BooleanProperty p) = True
ignoreArguments (ConditionalProperty p q) = ignoreArguments q
ignoreArguments (UniversalProperty f) = \x -> ignoreArguments (f x)

quickCheck :: Testable a b => a -> Q.Property
quickCheck p = quickCheckProperty (property p)

quickCheckProperty :: Property a -> Q.Property
quickCheckProperty (BooleanProperty p) = Q.property p
quickCheckProperty (ConditionalProperty p q) = p Q.==> quickCheckProperty q
quickCheckProperty (UniversalProperty f) = Q.property (\x -> quickCheckProperty (f x))

instance Q.Testable (Property a) where
  property = quickCheckProperty
