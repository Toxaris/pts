module PTS.Syntax.Telescope
  ( Telescope
  , foldTelescope
  ) where

import PTS.Syntax.Names

type Telescope t = [([Name], t)]

-- | Desugar a telescope in a binder with multiple arguments like this:
--
-- > lambda (x1 : e1) (x2 x3 : e2) . e
--
-- to a series of nested single argument binders:
--
-- > lambda x1 : e1 . lambda x2 : e2 . lambda x3 : e2 . e
foldTelescope :: (Name -> a -> b -> b) -> Telescope a -> b -> b
foldTelescope mk [] body = body
foldTelescope mk (([], _) : args) body = foldTelescope mk args body
foldTelescope mk (((n : ns), t) : args) body = mk n t (foldTelescope mk ((ns, t) : args) body)
