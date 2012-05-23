module PTS.QuasiQuote (pts) where

import PTS.AST
import PTS.Parser (parseTermAtPos)

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

pts  :: QuasiQuoter
pts  =  QuasiQuoter
  quoteExprExp
  quoteExprPat
  (error "cannot use pts quasiquoter in types")
  (error "cannot use pts quasiquoter in declarations")

quoteExprExp :: String -> TH.ExpQ
quoteExprExp text = do
  loc <- TH.location
  let  file  =  TH.loc_filename loc
       line  =  fst (TH.loc_start loc)
       col   =  snd (TH.loc_start loc)
  expr <- parseTermAtPos file line col text
  dataToExpQ (const Nothing `extQ` antiExprExp) expr

antiExprExp :: Term -> Maybe (TH.Q TH.Exp)
antiExprExp t = case structure t of
  Unquote v  ->  Just $ TH.varE (TH.mkName (show v))
  _          ->  Nothing

quoteExprPat :: String -> TH.PatQ
quoteExprPat text = do 
  loc <- TH.location
  let  file  =  TH.loc_filename loc
       line  =  fst (TH.loc_start loc)
       col   =  snd (TH.loc_start loc)
  expr <- parseTermAtPos file line col text
  dataToPatQ (const Nothing `extQ` antiExprPat) expr
 
antiExprPat :: Term -> Maybe (TH.Q TH.Pat)
antiExprPat t = case structure t of
  (Unquote v)  -> Just $ TH.varP  (TH.mkName (show v))
  _            -> Nothing
