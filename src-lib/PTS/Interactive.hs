module PTS.Interactive
           ( module PTS.Syntax.Term
           --, module PTS.Syntax -- too many parsing-related details
           , module PTS.Statics
           , module PTS.Dynamics
           , module PTS.QuasiQuote
           , module PTS.Interactive
           , module PTS.Process.Main
           , showPretty
           ) where

-- ASTs
import PTS.Syntax.Names (Name(..))
import PTS.Syntax.Term (Term(..), TypedTerm(..), TermStructure(..), BinOp(..))

import PTS.Syntax


import PTS.Dynamics
import PTS.Statics
import PTS.QuasiQuote
import PTS.Error
import PTS.Process.Main (processFileSimple)

parseSimple :: String -> Either [PTSError] Term
parseSimple input = parseTerm "REPL" input

nbeClosed :: Term -> Term
nbeClosed = nbe []
