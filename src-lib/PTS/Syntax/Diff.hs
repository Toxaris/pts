module PTS.Syntax.Diff where

import Control.Arrow ((***))

import Data.List (intersperse)
import Data.Set (Set)
import qualified Data.Set as Set

import PTS.Syntax.Algebra
import PTS.Syntax.Names
import PTS.Syntax.Pretty
import PTS.Syntax.Term
import PTS.Syntax.Substitution (freshCommonVar)

data Diff
  = DEqual Term
  | DDifferent Term Term
  | DIntOp BinOp Diff Diff
  | DIfZero Diff Diff Diff
  | DApp Diff Diff
  | DLam Name Diff Diff
  | DPi Name Diff Diff Bool Bool

allEqual :: [Diff] -> Bool
allEqual = all isEqual where
  isEqual (DEqual _) = True
  isEqual _ = False

diff :: Structure t => t -> t -> Diff
diff t1 t2 = case (structure t1, structure t2) of
  (Int n1, Int n2)
    |   n1 == n2   ->  DEqual (strip t1)
    |   otherwise  ->  DDifferent (strip t1) (strip t2)

  (IntOp op1 x1 y1, IntOp op2 x2 y2)
    |   op1 == op2 ->  let x = diff x1 x2; y = diff y1 y2 in
                         if allEqual [x, y] then DEqual (strip t1) else DIntOp op1 x y
    |   otherwise  ->  DDifferent (strip t1) (strip t2)

  (IfZero n1 x1 y1, IfZero n2 x2 y2)
    ->  let n = diff n1 n2; x = diff x1 x2; y = diff y1 y2 in
          if allEqual [n, x, y] then DEqual (strip t1) else DIfZero n x y

  (Const c1, Const c2)
    |   c1 == c2   ->  DEqual (strip t1)
    |   otherwise  ->  DDifferent (strip t1) (strip t2)

  (Var v1, Var v2)
    |   v1 == v2   ->  DEqual (strip t1)
    |   otherwise  ->  DDifferent (strip t1) (strip t2)

  (App f1 a1, App f2 a2)
    ->  let f = diff f1 f2; a = diff a1 a2 in
          if allEqual [f, a] then DEqual (strip t1) else DApp f a

  (Lam n1 q1 b1, Lam n2 q2 b2)
    ->  let  (n, b1', b2') = freshCommonVar n1 n2 (strip b1) (strip b2)
             q = diff q1 q2
             b = diff b1' b2' in
          if allEqual [q, b]
            then DEqual (mkLam n (strip q1) b1')
            else DLam n q b

  (Pi n1 q1 b1 _, Pi n2 q2 b2 _)
    ->  let  (n, b1', b2') = freshCommonVar n1 n2 (strip b1) (strip b2)
             q = diff q1 q2
             b = diff b1' b2' in
          if allEqual [q, b]
            then DEqual (mkPi n (strip q1) b1')
            else DPi n q b (n `Set.member` freevars b1') (n `Set.member` freevars b2')

  (Pos p t, _)
    ->  diff t t2

  (_, Pos p t)
    ->  diff t1 t

  (_, _)
    ->  DDifferent (strip t1) (strip t2)

showToplevel :: Int -> Term -> String
showToplevel p t = singleLine (pretty p t)

showNothing :: Int -> Term -> String
showNothing p t = if null (drop 5 s) then s else "..." where
  s = singleLine (pretty p t)

prio :: Int -> Int -> (String, String) -> (String, String)
prio m n (x, y) | m < n = ("(" ++ x ++ ")", "(" ++ y ++ ")")
                | otherwise = (x, y)

type LamChain = [(Name, Diff)]

findLamChain :: Diff -> (LamChain, Diff)
findLamChain (DLam n q b) = ((n, q) : args, body) where
  (args, body) = findLamChain b
findLamChain x = ([], x)

showLamChain :: LamChain -> (String, String)
showLamChain = (concat *** concat) . unzip . intersperse (" ", " ") . map showElement where
  showElement (name, DEqual _) = (showName name, showName name)
  showElement (name, d) = ("(" ++ showName name ++ " : " ++ q1 ++ ")",
                           "(" ++ showName name ++ " : " ++ q2 ++ ")") where
    (q1, q2) = showDiff 0 d

showDiff :: Int -> Diff -> (String, String)

showDiff p (DEqual t) = (showNothing p t, showNothing p t)

showDiff p (DDifferent t1 t2) = (t1' ++ spaces1,
                                 t2' ++ spaces2) where
  spaces1 = replicate (max 0 (length t2' - length t1')) ' '
  spaces2 = replicate (max 0 (length t1' - length t2')) ' '
  t1' = showToplevel p t1
  t2' = showToplevel p t2

showDiff p (DIntOp n x y) = prio 2 p
                            (show n ++ " " ++ x1 ++ " " ++ y1,
                             show n ++ " " ++ x2 ++ " " ++ y2) where
  (x1, x2) = showDiff 3 x
  (y1, y2) = showDiff 3 y

showDiff p (DIfZero n x y) = prio 2 p
                             ("if0 " ++ n1 ++ " " ++ x1 ++ " " ++ y1,
                              "if0 " ++ n2 ++ " " ++ x2 ++ " " ++ y2) where
  (n1, n2) = showDiff 3 n
  (x1, x2) = showDiff 3 x
  (y1, y2) = showDiff 3 y

showDiff p (DApp f a) = prio 2 p
                        (f1 ++ " " ++ a1,
                         f2 ++ " " ++ a2) where
  (f1, f2) = showDiff 2 f
  (a1, a2) = showDiff 3 a

showDiff p (DLam n q b) = prio 0 p
                          ("lambda " ++ q1 ++ " . " ++ b1,
                           "lambda " ++ q2 ++ " . " ++ b2) where
  (chain, body) = findLamChain b
  (q1, q2) = showLamChain ((n, q) : chain)
  (b1, b2) = showDiff 0 body

showDiff p (DPi n q b True True) = prio 0 p
                         ("Pi " ++ showName n ++ " : " ++ q1 ++ " . " ++ b1,
                          "Pi " ++ showName n ++ " : " ++ q2 ++ " . " ++ b2) where
  (q1, q2) = showDiff 0 q
  (b1, b2) = showDiff 0 b

showDiff p (DPi n q b True False) = prio 1 p
                         ("Pi " ++ showName n  ++ " : " ++ q1 ++ " .  " ++ b1,
                          "   " ++      n2 ++ "   " ++ q2 ++ " -> " ++ b2) where
  (q1, q2) = showDiff 2 q
  (b1, b2) = showDiff 1 b
  n2 = replicate (length (showName n)) ' '

showDiff p (DPi n q b False True) = prio 1 p
                         ("   " ++      n1 ++ "   " ++ q1 ++ " -> " ++ b1,
                          "Pi " ++ showName n  ++ " : " ++ q2 ++ " .  " ++ b2) where
  (q1, q2) = showDiff 2 q
  (b1, b2) = showDiff 1 b
  n1 = replicate (length (showName n)) ' '

showDiff p (DPi n q b False False) = prio 1 p
                         (q1 ++ " -> " ++ b1,
                          q2 ++ " -> " ++ b2) where
  (q1, q2) = showDiff 2 q
  (b1, b2) = showDiff 1 b
