{-# LANGUAGE ScopedTypeVariables #-}
-- Monadic runners

module PTS.Interactive.Runners where

import PTS.Process.Main
import PTS.Options

import Control.Monad.Errors
import Control.Monad.Environment
import Control.Monad.Assertions (checkAssertions)

-- Typechecking needs an environment monad to read bindings.
typecheckWrapper action env inst =
  runErrorsAndOpts inst $ runEnvironmentT action env

-- Instead, higher level actions need a state monad. But that does not hurt too much for typechecking.
runErrorsAndOpts inst | False =
  -- Force the return monad to be IO, to simplify type signatures at call site.
  -- Inspired by http://okmij.org/ftp/Haskell/partial-signatures.lhs
  -- Should be easier with GHC 7.8/7.10.
  const returnsIO
runErrorsAndOpts inst =
  withEmptyState . runErrorsT . checkAssertions . runOptMonads (optionsForInstance inst)

returnsIO :: IO a
returnsIO = undefined

optionsForInstance Nothing = defaultOptions
optionsForInstance (Just inst) = setInstance inst $ optionsForInstance Nothing
