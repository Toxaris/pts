{-# LANGUAGE QuasiQuotes #-}
module Main where

import PTS.Transform
import PTS.QuasiQuote

main = transform f

f e = [pts| lambda e : $se . lambda R : * -> * .
              reify R $se $(cv e) $(rr e) $(lf ne e) |] where
  ne = [pts| e |]
  se = strip e
  rr t = case structure t of
    Pos _ t -> rr t
    Pi n t1 t2 -> case asSort (typeOf t1) of
      Term -> [pts| rrFun R
                      $(strip t1) $(cv t1)
                      $(strip t2) $(cv t2)
                      $(rr t1) $(rr t2) |]
      Type -> [pts| rrAll R $(strip t1)
                      (lambda $n : $(strip t1) . $(strip t2))
                      (lambda $n : $(strip t1) . $(cv t2))
                      (lambda $n : $(strip t1) . $(rr t2)) |]
    _ -> [pts| rrBase R $(strip t) |]

  cv t = case structure t of
    Pos _ t -> cv t
    Pi n t1 t2 -> case asSort (typeOf t1) of
      Term -> [pts| $(cv t1) -> $(cv t2) |]
      Type -> [pts| Pi $n : $(strip t1) . $(cv t2) |]
    _ -> [pts| Mono R $(strip t) |]

  lf e te = case structure te of
    Pos _ te -> lf e te
    Pi n t1 t2 -> case asSort (typeOf t1) of
      Term -> let  body = [pts| $e $(mkVar n) |]
                in [pts| lambda $n : $(cv t1) . $(lf body t2) |]
      Type -> let  -- TODO support typeOf t1 /= *
                   body = [pts| $e (Mono R $(mkVar n)) |]
                in [pts| lambda $n : $(strip t1) . $(lf body t2) |]
    other -> [pts| $e |]

