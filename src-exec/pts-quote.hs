{-# LANGUAGE QuasiQuotes #-}
module Main where

import PTS.Transform
import PTS.QuasiQuote
import PTS.Constants (C (C))

main = transform quote

quote t = case sortOf t of
  Term -> quoteTerm t
  Type -> quoteType t

quoteType t = strip t

quoteTerm t = [pts|lambda $repName   :  * -> * .
                   lambda $appName   :  Pi A : * . Pi B : * .
                                          $rep (A -> B) -> $rep A -> $rep B .
                   lambda $absName   :  Pi A : * . Pi B : * .
                                          ($rep A -> $rep B) -> $rep (A -> B) .
                   lambda $tappName  :  Pi A : ** . Pi B : A -> * .
                                          $rep (Pi X : A . B X) -> Pi X : A . $rep (B X) .
                   lambda $tabsName  :  Pi A : ** . Pi B : A -> * .
                                          (Pi X : A . $rep (B X))  ->  $rep (Pi X : A . B X) .
                     $body|] where
  repName   =   read "R"
  appName   =   read "app"
  absName   =   read "abs"
  tappName  =   read "tapp"
  tabsName  =   read "tabs"

  rep   =  mkVar repName
  app   =  mkVar appName
  abs   =  mkVar absName
  tapp  =  mkVar tappName
  tabs  =  mkVar tabsName

  body = q t

  q t = case structure t of
    Var n ->  mkVar n
    App t1 t2 -> case structure (typeOf t1) of
      Pi n t3 t4 -> case sortOf t2 of
        Term  ->  [pts| $app  $(strip t3) $(strip t4) $(q t1) $(q t2) |]
        Type  ->  [pts| $tapp $(strip t3) (lambda $n : $(strip t3) . $(strip t4)) $(q t1) $(q t2) |]
    Lam n1 t1 t2 -> case structure (typeOf t) of
      Pi n2 t3 t4 -> case asSort (typeOf t1) of
        Term  -> [pts| $abs  $(strip t3) $(strip t4) (lambda $n1 : $rep $(strip t1) . $(q t2)) |]
        Type  -> [pts| $tabs $(strip t3) (lambda $n2 : $(strip t3) . $(strip t4)) (lambda $n1 : $(strip t1) . $(q t2)) |]
    Pos p t -> q t
