module PTS.Syntax.Telescope
  ( Telescope
  , desugarArgs
  ) where

import PTS.Syntax.Names

type Telescope t = [([Name], t)]

-- | Desugar a binder with multiple arguments like this:
--
-- > lambda (x1 : e1) (x2 x3 : e2) . e
--
-- to a series of nested single argument binders:
--
-- > lambda x1 : e1 . lambda x2 : e2 . lambda x3 : e2 . e
desugarArgs :: (Name -> a -> b -> b) -> Telescope a -> b -> b
desugarArgs mk [] body = body
desugarArgs mk (([], _) : args) body = desugarArgs mk args body
desugarArgs mk (((n : ns), t) : args) body = mk n t (desugarArgs mk ((ns, t) : args) body)
