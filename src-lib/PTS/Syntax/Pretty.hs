{-# LANGUAGE NoMonomorphismRestriction #-}
module PTS.Syntax.Pretty
  ( singleLine
  , multiLine
  , showCtx
  , prettyAlgebra
  , showAssertion
  ) where

import Control.Arrow (first)

import Data.List (intersperse, intercalate)
import Data.Set (Set)
import qualified Data.Set as Set

import Parametric.Pretty

import PTS.Syntax.Algebra
import PTS.Syntax.Constants
import PTS.Syntax.Names
import PTS.Syntax.Statement
import PTS.Syntax.Term

-- priorities
pAppR = 3
pApp  = 2
pArrL = 2
pArr  = 1
pLam  = 1
pPi   = 1
pIf0  = 2

-- pretty printing
instance Pretty Name where
  pretty _ name = text (show name)

instance Pretty C where
  pretty _ (C 0) = text "Int"
  pretty _ (C n) = text (replicate n '*')

data PrettyChain
  = AppChain Bool Doc [Doc]
  | LamChain [(Doc, Doc)] Doc
  | PiChain [(Doc, Doc)] Doc
  | ArrChain [Doc] Doc
  | Atomic Doc
  | Composite Priority Doc

type Priority = Int

instance Pretty PrettyChain where
  pretty p (AppChain atomic operator arguments) =
    parens `when` (pApp < p) $
      (if atomic then hsep else sep)
        [operator, nest 2 . sep . reverse $ arguments]

  pretty p (LamChain lams body) =
    parens `when` (pLam < p) $
      sep [ sep . map (\(n, q) -> text "lambda" <+> pretty 0 n <+> text ":" <+> q <+> text ".") $ lams
          , nest 2 $ body ]

  pretty p (PiChain pis body) =
    parens `when` (pPi < p) $
      sep [ sep . map (\(n, q) -> text "Pi" <+> n <+> text ":" <+> q <+> text ".") $ pis
          , nest 2 $ body ]

  pretty p (ArrChain arrs body) =
    parens `when` (pArr < p) $
      sep $ [sep . map (\q -> q <+> text "->") $ arrs, body] -- TODO is this correct?

  pretty p (Atomic t) =
    t

  pretty p (Composite p' t) =
    parens `when` (p' < p) $
      t

prettyAlgebra :: PreAlgebra (Names, PrettyChain) PrettyChain
prettyAlgebra (Int n) = Atomic $
  integer n

prettyAlgebra (IntOp n _ (_, a) (_, b)) = Composite pApp $
  pretty 0 n <+> pretty pAppR a <+> pretty pAppR b

prettyAlgebra (IfZero (_, c) (_, t) (_, e)) = Composite pIf0 $
  sep [ text "if0" <+> pretty pIf0 c
      , nest 2 $ text "then" <+> pretty pIf0 t
      , nest 2 $ text "else" <+> pretty pIf0 e ]

prettyAlgebra (Var n) = Atomic $
  pretty 0 n

prettyAlgebra (Const c) = Atomic $
  pretty 0 c

prettyAlgebra (App (_, AppChain atomic operator arguments) (_, argument)) =
  AppChain atomic operator (pretty pAppR argument : arguments)

prettyAlgebra (App (_, Atomic operator) (_, argument)) =
  AppChain True (pretty pApp operator) [pretty pAppR argument]

prettyAlgebra (App (_, operator) (_, argument)) =
  AppChain False (pretty pApp operator) [pretty pAppR argument]

prettyAlgebra (Lam name (_, qualifier) (_, LamChain lams body)) =
  LamChain ((pretty 0 name, pretty pLam qualifier) : lams) body

prettyAlgebra (Lam name (_, qualifier) (_, body)) =
  LamChain [(pretty 0 name, pretty pLam qualifier)] (pretty pLam body)

prettyAlgebra (Pi name (_, qualifier) (freevars, PiChain lams body)) | name `Set.member` freevars =
  PiChain ((pretty 0 name, pretty pPi qualifier) : lams) body

prettyAlgebra (Pi name (_, qualifier) (freevars, body)) | name `Set.member` freevars =
  PiChain [(pretty 0 name, pretty pPi qualifier)] (pretty pPi body)

prettyAlgebra (Pi name (_, qualifier) (freevars, ArrChain lams body)) | name `Set.notMember` freevars =
  ArrChain (pretty pArrL qualifier : lams) body

prettyAlgebra (Pi name (_, qualifier) (freevars, body)) | name `Set.notMember` freevars =
  ArrChain [pretty pArrL qualifier] (pretty pArr body)

prettyAlgebra (Pos _ (_, t)) = t

instance Pretty Term where
  pretty p t = pretty p (snd (fold (depZip freevarsAlgebra prettyAlgebra) t))

instance Pretty TypedTerm where
  pretty p t = pretty p (snd (fold (depZip freevarsAlgebra prettyAlgebra) t))

prettyArgs :: [([Name], Term)] -> Doc
prettyArgs args = sep (map f args) where
  f (ns, q) = hsep [pretty 0 n | n <- ns] <+> text ":" <+> pretty 0 q

instance Pretty Stmt where
  pretty p (Bind n args Nothing t)   = pretty 0 n <+> prettyArgs args <+> text "=" <+> pretty 0 t
  pretty p (Bind n args (Just t') t) = pretty 0 n <+> prettyArgs args <+> text ":" <+> pretty 0 t' <+> text "=" <+> pretty 0 t
  pretty p (Term t) = pretty 0 t
  pretty p (Assertion t q' t') = text "assert" <+> prettyAssertion t q' t'
  pretty p (Import n) = text "import" <+> pretty 0 n
  pretty p (Export mod) = text "export" <+> pretty 0 mod

prettyAssertion t q' t' =
  pretty 0 t <+> pq' <+> pt' where
    pq' = case q' of
      Nothing -> empty
      Just q' -> text ":" <+> pretty 0 q'
    pt' = case t' of
      Nothing -> empty
      Just t' -> text "=" <+> pretty 0 t'

showAssertion t q' t' = singleLine (prettyAssertion t q' t')

instance Pretty ModuleName where
  pretty p m = text (intercalate "." (parts m))

instance Show Term where
  show t = singleLine t

instance Show TypedTerm where
  show t = singleLine t

instance Show ModuleName where
  show t = singleLine t

showCtx :: [(Name, (a, TypedTerm))] -> String
showCtx = concat . intersperse ", " . map (\(n, (v, t)) -> show n ++ " : " ++ show t)
