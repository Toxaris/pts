{-# LANGUAGE FlexibleInstances #-}
module Parametric.Pretty
  ( Pretty (..)
  , when
  , module Text.PrettyPrint.HughesPJ
  , singleLine
  , multiLine
  ) where

import Text.PrettyPrint.HughesPJ

class Pretty p where
  pretty :: Int -> p -> Doc

instance Pretty [Char] where
  pretty _ = text

instance Pretty Doc where
  pretty _ x = x

when f True = f
when f False = id

singleLine :: Pretty p => p -> String
singleLine p = renderStyle (Style OneLineMode 80 1.0) (pretty 0 p)

multiLine :: Pretty p => Int -> p -> String
multiLine n p = renderStyle (Style PageMode n 1.5) (pretty 0 p)

