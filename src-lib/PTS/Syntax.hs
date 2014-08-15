module PTS.Syntax
  ( -- * Abstract syntax
    -- ** Names
    Name
  , Names
  , freshvarl
  , LanguageName
  , ModuleName (..)
  , parts
    -- ** Constants
  , C (C)
  , int
  , star
  , box
  , triangle
  , circle
    -- ** Arithmetic operators
  , BinOp (Add, Sub, Mul, Div)
  , evalOp
    -- ** Term structure
  , TermStructure (..)
  , Structure (structure)
  , structure'
    -- ** Folding
  , PreAlgebra
  , Algebra
  , fold
  , depZip
    -- ** Plain terms
  , Term
  , strip
  , mkInt
  , mkIntOp
  , mkIfZero
  , mkVar
  , mkConst
  , mkApp
  , mkLam
  , mkPi
  , mkSortedPi
  , mkPos
  , mkUnquote
  , mkInfer
  , handlePos
    -- ** Annotated terms
  , AnnotatedTerm
  , annotation
  , annotatedHandlePos
    -- ** Typed terms
  , TypedTerm
  , typeOf
  , sortOf
  , typedHandlePos
    -- ** Telescopes
  , Telescope
  , foldTelescope
    -- ** Statements
  , Stmt (..)
    -- ** Files
  , File (..)
    -- * Concrete syntax
    -- ** Parser
  , parseFile
  , parseStmt
  , parseStmts
  , parseTerm
  , parseTermAtPos
    -- ** Printer
  , Pretty (pretty)
  , singleLine
  , multiLine
  , showPretty
  , showCtx
  , showAssertion
    -- * Operations
  , allvars
  , freevars
  , freshvar
  , Diff
  , diff
  , showDiff
  , subst
  -- , typedSubst
    -- * Algebras
  , allvarsAlgebra
  , freevarsAlgebra
  , prettyAlgebra
  ) where

import PTS.Syntax.Algebra
import PTS.Syntax.Constants
import PTS.Syntax.Diff
import PTS.Syntax.File
import PTS.Syntax.Names
import PTS.Syntax.Parser
import PTS.Syntax.Pretty
import PTS.Syntax.Statement
import PTS.Syntax.Substitution
import PTS.Syntax.Telescope
import PTS.Syntax.Term

import PTS.Dynamics.TypedTerm
