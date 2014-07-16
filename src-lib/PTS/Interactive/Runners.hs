{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
-- Monadic runners

module PTS.Interactive.Runners where

import PTS.Process.Main
import PTS.Options

import Control.Monad.State
import Control.Monad.Errors
import Control.Monad.Environment
import Control.Monad.Assertions (checkAssertions)

-- Typechecking needs an environment monad to read bindings.
typecheckWrapper inst action env =
  runErrorsAndOpts inst $ runEnvironmentT action env

-- Instead, higher level actions need a state monad. But that does not hurt too much for typechecking.

-- Force the return monad to be IO, to simplify type signatures at call site.
-- Inspired by http://okmij.org/ftp/Haskell/partial-signatures.lhs
-- Should be easier with GHC 7.8/7.10.
runErrorsAndOpts inst | False =
  const returnsIO
runErrorsAndOpts inst =
  runErrorsT . withEmptyState . runAssertAndOptMonads inst
-- XXX Consider replacing runErrorsAndOpts with runErrorsAndOptsGetState

runErrorsAndOptsGetState inst | False =
  const returnsIO
runErrorsAndOptsGetState inst =
  runErrorsT . observeFinalState . runAssertAndOptMonads inst

runAssertAndOptMonads inst = checkAssertions . runOptMonads (optionsForInstance inst)

returnsIO :: IO a
returnsIO = undefined

optionsForInstance Nothing = defaultOptions
optionsForInstance (Just inst) = setInstance inst $ optionsForInstance Nothing

observeFinalState = flip runStateT initState
