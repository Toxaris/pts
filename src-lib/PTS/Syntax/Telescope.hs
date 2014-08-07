module PTS.Syntax.Telescope
  ( Telescope
  ) where

import PTS.Syntax.Names

type Telescope t = [([Name], t)]
