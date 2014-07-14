module PTS.Interactive
           ( module PTS.Syntax.Term
           --, module PTS.Syntax -- too many parsing-related details
           , module PTS.Statics
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

import Control.Monad.Errors
import Control.Monad.Environment

import PTS.Dynamics
import PTS.Statics
import PTS.Instances
import PTS.QuasiQuote
import PTS.Error
import PTS.Process.Main
import PTS.Process.File
import PTS.Options
import Data.Map (Map)
import Data.Maybe

parseSimple :: String -> Either [PTSError] Term
parseSimple input = parseTerm "REPL" input

nbeClosed :: Term -> Term
nbeClosed = nbe []

optionsForInstance Nothing = defaultOptions
optionsForInstance (Just inst) = setInstance inst $ optionsForInstance Nothing

runMoreMonads action inst =
  runErrorsT . runMainState $ simpleRunMonads action (optionsForInstance inst)

processFileSimple
  :: FilePath -> Maybe PTS -> IO (Either [PTSError] (Maybe (Module Eval)))
processFileSimple f inst = runMoreMonads (processFile f) inst

processFileSimpleInt
  :: FilePath -> Maybe PTS -> IO (Either [PTSError] (Maybe ModuleName, (Map ModuleName (Module Eval), [ModuleName], Bindings Eval)))
processFileSimpleInt f inst = runMoreMonads (processFileInt f) inst

processStmtSimple stmt inst = runMoreMonads (processStmt stmt) inst

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

typecheckWrapper action env inst =
  runErrorsT $ simpleRunMonads (runEnvironmentT action env) (optionsForInstance inst)


wrapTypecheckPull term =
  typecheckWrapper (typecheckPull term)

-- expectedType must already have been typechecked. XXX add wrapper which does that too?
wrapTypecheckPush term expectedType =
  typecheckWrapper (typecheckPush term expectedType)
