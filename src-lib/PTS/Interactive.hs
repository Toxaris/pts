-- Entry point for `cabal repl`.
--
-- Calling :browse should give a nice overview of the API for interactive use.
--
-- To this end, I try to have in scope just enough stuff to minimize qualified
-- names there, and to monomorphise signatures for higher readability.

module PTS.Interactive
           ( module PTS.Syntax.Term
           --, module PTS.Syntax -- too many parsing-related details
           --, module PTS.Statics:
           --, normalizeToSort
           , module PTS.Dynamics
           , module PTS.QuasiQuote
           , module PTS.Interactive
           , showPretty
           -- From PTS.Instances
           , PTS
           , coc
           , fomegastar
           ) where

-- ASTs
import PTS.Syntax.Names (Name(..))
import PTS.Syntax.Term (Term(..), TypedTerm(..), TermStructure(..), BinOp(..))

import PTS.Syntax

import PTS.Dynamics
import PTS.Statics
import PTS.Instances
import PTS.QuasiQuote
import PTS.Error
import PTS.Process.File
import PTS.Interactive.Runners

import Data.Map (Map)
import Data.Maybe

parseSimple :: String -> Either [PTSError] Term
parseSimple input = parseTerm "REPL" input

nbeClosed :: Term -> Term
nbeClosed = nbe []

processFileSimple f inst = runErrorsAndOpts inst (processFile f)
processFileSimpleInt f inst = runErrorsAndOpts inst (processFileInt f)
processStmtSimple stmt inst = runErrorsAndOpts inst (processStmt stmt)

-- r ^. _Right . _2 . _3
getBindings :: Either [PTSError] (Maybe ModuleName, (Map ModuleName (Module Eval), [ModuleName], Bindings Eval)) -> Bindings Eval
getBindings (Right (_, (_, _, bindings))) = bindings
getBindings _ = []

wrapTypecheckPull ::
  Term
  -> Bindings Eval -- Env Name (Binding Eval)
  -> Maybe PTS
  -> IO (Either [PTSError] TypedTerm)
wrapTypecheckPush ::
  Term
  -> TypedTerm
  -> Bindings Eval
  -> Maybe PTS
  -> IO (Either [PTSError] TypedTerm)

wrapTypecheckPull term =
  typecheckWrapper (typecheckPull term)

-- expectedType must already have been typechecked. XXX add wrapper which does that too?
wrapTypecheckPush term expectedType =
  typecheckWrapper (typecheckPush term expectedType)
