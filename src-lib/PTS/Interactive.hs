-- Entry point for `cabal repl`.
--
-- Calling :browse should give a nice overview of the API for interactive use.
--
-- To this end, I try to have in scope just enough stuff to minimize qualified
-- names there, and to monomorphise signatures for higher readability.
--
-- Moreover, I use qualified imports just to have shorter names in :browse.

module PTS.Interactive
           ( module PTS.Interactive -- Everything defined here.
           , module PTS.Syntax.Term
           --, module PTS.Syntax -- too many parsing-related details
           --, module PTS.Statics -- we export wrapped functions
           , module PTS.Dynamics
           , module PTS.QuasiQuote
           , showPretty
           -- Qualified names from PTS.Instances
           , Instances.coc
           , Instances.fomegastar
           ) where

-- ASTs
import PTS.Syntax.Names (Name(..))
import PTS.Syntax.Term (Term(..), TypedTerm(..), TermStructure(..), BinOp(..))

import PTS.Syntax

import PTS.Statics
import PTS.QuasiQuote

import PTS.Error

import PTS.Process.File
import PTS.Interactive.Runners

import qualified PTS.Instances as Instances
import PTS.Dynamics
import qualified PTS.Dynamics.Value as Value
import qualified PTS.Dynamics.Evaluation as Evaluation

import Data.Map (Map)
import Data.Maybe

parseSimple :: String -> Either Errors Term
parseSimple input = parseTerm "REPL" input

parseStSimple :: String -> Either Errors Stmt
parseStSimple input = parseStmt "REPL" input

nbeClosed :: Term -> Term
nbeClosed = nbe []

processFileSimple inst f = runErrorsAndOpts inst (processFile f)
processFileSimpleInt inst f = runErrorsAndOpts inst (processFileInt f)
processStmtSimple inst stmt = runErrorsAndOptsGetState inst (processStmt stmt)

-- With lens, this is r ^. _Right . _2 . _3
getBindings :: Either Errors (Maybe ModuleName, (Map ModuleName (Module Eval), [ModuleName], Bindings Eval)) -> Bindings Eval
getBindings (Right (_, (_, _, bindings))) = bindings
getBindings _ = []

wrapTypecheckPull ::
  Maybe Instances.PTS
  -> Term
  -> Bindings Eval
  -> IO (Either Errors TypedTerm)
wrapTypecheckPush ::
  Maybe Instances.PTS
  -> Term
  -> TypedTerm
  -> Bindings Eval
  -> IO (Either Errors TypedTerm)
wrapTypecheckPushUntyped ::
  Maybe Instances.PTS
     -> Term
     -> Term
     -> Bindings Eval
     -> IO (Either Errors TypedTerm)

wrapTypecheckPull inst term =
  typecheckWrapper inst (typecheckPull term)

-- expectedType must already have been typechecked. `typecheckPushUntyped` does that for you.
wrapTypecheckPush inst term expectedType =
  typecheckWrapper inst (typecheckPush term expectedType)

wrapTypecheckPushUntyped inst term untypedExpectedType =
  typecheckWrapper inst (typecheckPushUntyped term untypedExpectedType)
