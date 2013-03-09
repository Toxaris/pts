{-# LANGUAGE ExistentialQuantification, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module PTS.Options where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans

import Data.Char
import Data.List (mapAccumL, intercalate)

import System.Console.GetOpt
import System.Environment (getArgs)

import Parametric.Pretty (Pretty, Doc, multiLine)

import PTS.Instances

-- options record
data Options = Options
  { optColumns :: Int
  , optInstance :: PTS
  , optLiterate :: Bool
  , optShowFullTerms :: Bool
  , optDebugQuote :: Bool
  , optDebugType :: Bool
  , optQuiet :: Bool
  }

-- monadic option combinators
whenOption :: (MonadReader Options m) => (Options -> Bool) -> m () -> m ()
whenOption  f act = ask >>= \opt            -> when (f opt) act

prettyFail :: MonadReader Options m => Doc -> m a
prettyFail doc = asks (flip multiLine doc . optColumns) >>= fail

-- default options
defaultOptions = Options
  { optColumns = 80
  , optInstance = fomegastar
  , optLiterate = False
  , optShowFullTerms = False
  , optDebugQuote = False
  , optDebugType = False
  , optQuiet = False
  }

setColumns    f1 (Options _  f2 f3 f4 f5 f6 f7) = Options f1 f2 f3 f4 f5 f6 f7
setInstance   f2 (Options f1 _  f3 f4 f5 f6 f7) = Options f1 f2 f3 f4 f5 f6 f7
setLiterate   f3 (Options f1 f2 _  f4 f5 f6 f7) = Options f1 f2 f3 f4 f5 f6 f7
setDebugTerms f4 (Options f1 f2 f3 _  f5 f6 f7) = Options f1 f2 f3 f4 f5 f6 f7
setDebugQuote f5 (Options f1 f2 f3 f4 _  f6 f7) = Options f1 f2 f3 f4 f5 f6 f7
setDebugType  f6 (Options f1 f2 f3 f4 f5 _  f7) = Options f1 f2 f3 f4 f5 f6 f7
setQuiet      f7 (Options f1 f2 f3 f4 f5 f6 _ ) = Options f1 f2 f3 f4 f5 f6 f7

data Flag
  = Error String
  | Help
  | Global (Options -> Options)
  | Local (Options -> Options)
  | FilePath FilePath
  | ShowInsts

-- option descriptions
options =
  [ Option ['c'] ["columns"]         (ReqArg handleColumns  "c"     ) "wrap output at specified column"
  , Option ['p'] ["pts", "instance"] (ReqArg handlePTS      "i"     ) "implement specified pure type systems instance"
  , Option ['l'] ["literate"]        (OptArg handleLiterate "b"     ) "treat input as literate source files"
  , Option ['d'] ["debug"]           (ReqArg handleDebug    "option") "activate specified debug options"
  , Option ['q'] ["quiet"]           (NoArg  handleQuiet            ) "don't print so much"
  , Option ['s'] ["show-instances"]  (NoArg  handleShowInsts        ) "show built-in instances"
  , Option "?h"  ["help"]            (NoArg  handleHelp             ) "display this help"
  ]

-- option processing
handleHelp         = Help

handleColumns  arg = case reads arg of
                       [(n, "")]     -> Local  (setColumns    n         )
                       _             -> Error  ("Error: columns option expects integer instead of " ++ arg)

handlePTS      arg = case map toLower arg of
                       str | nameOf lama -> Global (setInstance lama)
                           | nameOf lam2 -> Global (setInstance lam2)
                           | nameOf lamp -> Global (setInstance lamp)
                           | nameOf lamv -> Global (setInstance lamv)
                           | nameOf lap2 -> Global (setInstance lap2)
                           | nameOf lapv -> Global (setInstance lapv)
                           | nameOf lamc -> Global (setInstance lamc)
                           | nameOf lams -> Global (setInstance lams)
                           | nameOf laws -> Global (setInstance laws)
                           | nameOf lawu -> Global (setInstance lawu)
                           | otherwise   -> Error  ("Error: Unknown pure type system instance " ++ arg)
                           where nameOf = elem str . name

handleLiterate arg = case fmap (map toLower) arg of
                       Nothing       -> Local  (setLiterate   True      )
                       Just "yes"    -> Local  (setLiterate   True      )
                       Just "no"     -> Local  (setLiterate   False     )
                       Just other    -> Error  ("Error: literate option expects 'yes' or 'no' instead of " ++ other)

handleDebug arg    = case map toLower arg of
                       "toplevel"    -> Local  (setDebugTerms True       )
                       "typing"      -> Local  (setDebugType  True       )
                       "quoting"     -> Local  (setDebugQuote True       )
                       _             -> Error  ("Error: debug option expects 'toplevel', 'typing' or 'quoting' instead of " ++ arg)

handleQuiet        = Global (setQuiet True)

handleShowInsts    = ShowInsts

-- order requirements
argOrder = ReturnInOrder FilePath

-- flag processing

processFlagsHelp []         = return ()
processFlagsHelp (Help : _) = liftIO printHelp
processFlagsHelp (_ : rest) = processFlagsHelp rest

processFlagsErrors     []                   = return ()
processFlagsErrors     (Error msg  : flags) = fail msg
processFlagsErrors     (_          : flags) = processFlagsErrors flags

processFlagsGlobal opt []                   = return opt
processFlagsGlobal opt (Global f   : flags) = processFlagsGlobal (f opt) flags
processFlagsGlobal opt (_          : flags) = processFlagsGlobal opt flags

processFlagsLocal  opt []                   = return []
processFlagsLocal  opt (Local f    : flags) = processFlagsLocal (f opt) flags
processFlagsLocal  opt (FilePath p : flags) = fmap ((opt, p) :) (processFlagsLocal opt flags)
processFlagsLocal  opt (_          : flags) = processFlagsLocal opt flags

processFlagsShowInsts []              = return ()
processFlagsShowInsts (ShowInsts : _) = liftIO printInstances
processFlagsShowInsts (_ : rest)      = processFlagsShowInsts rest

printHelp = putStrLn (usageInfo "PTS interpreter" options)

printInstances :: IO ()
printInstances = putStrLn instInfo
  where instInfo = unlines $ map (\i -> intercalate ", " (name i) ++ "\n  -- " ++ description i) insts

insts :: [PTS]
insts = [lama, lam2, lamp, lamv, lap2, lapv, lamc, lams, laws, lawu]

-- main entry point
parseCommandLine :: (Functor m, MonadIO m) => ([(Options, FilePath)] -> m a) -> m ()
parseCommandLine handler = do
  cmdline <- liftIO getArgs
  let (flags, [], errors) = getOpt argOrder options cmdline
  mapM_ (fail . ("Syntax Error in command line: " ++)) errors
  processFlagsHelp flags
  processFlagsErrors flags
  processFlagsShowInsts flags
  global <- processFlagsGlobal defaultOptions flags
  jobs <- processFlagsLocal global flags
  handler jobs
  return ()
