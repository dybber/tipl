{-# LANGUAGE
    TypeOperators
  , TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}
module Tree where

import Data.List(tails, intersperse)

import Language
import MSG

data Tree = Branch { treeConf :: Conf, treeBranches :: [(Contr, Tree)] }

instance Show Tree where
  show t = show' "" t
    where show' ind (Branch c brs) =
              show c ++ "\n" ++ (concat $ map (showBr (' ':' ':' ':ind)) brs)
          showBr ind (Left ineq, tr) = ind ++ (show ineq) ++ " : " ++ (show' ind tr)
          showBr ind (Right subs, tr) = ind ++ (showS subs) ++ " : " ++ (show' ind tr)
          showS [] = "Ø"
          showS subs = concat $ intersperse ", " $ map (\(a,b) -> show a ++ " |-> " ++ show b) subs
type Node = [Int]

leaf :: Tree -> [Node]
leaf = map reverse . leaf' []
    where leaf' p (Branch _ []) = [p]
          leaf' p (Branch _ bs) = do
            (ix, (_, br)) <- zip [1..] bs
            leaf' (ix:p) br

instance Subst Tree (Node :-> Tree) Tree where
  (Branch _ _) ./ ([], t') = t'
  (Branch c brs) ./ (ix:ixs, t') = Branch c $ substBr ix brs
      where substBr 1 ((ctr, br):brs') = (ctr, br ./ (ixs, t')):brs'
            substBr ix' (br:brs') = br:substBr (ix'-1) brs'
            substBr _  _ = error "node label out of bounds"

anc :: Node -> [Node]
anc [] = []
anc n = map reverse . drop 1 . tails . reverse $ n

sub :: Tree -> Node -> Tree
sub br@(Branch _ _) [] = br
sub (Branch _ brs) (ix:ixs) = sub (snd (brs !! (ix-1))) ixs

isProcessed :: Tree -> Node -> Bool
isProcessed tr n = theta == [] && (isExpTerm t || any (== t) ancTerms)
  where Branch (Let theta t) _ = sub tr n
        ancTerms = map letTerm $ filter (not . isProper) $ map (treeConf . sub tr) $ anc n

isProperN :: Tree -> Node -> Bool
isProperN t n = isProper . treeConf $ sub t n

abstract :: FreshVars C -> Tree -> Node -> Node -> (FreshVars C, Tree)
abstract fr t a b =
  let Branch (Let [] t1) _ = sub t a
      Branch (Let [] t2) _ = sub t b
      Just (fr', t_g, theta, _) = msg fr t1 t2
      Just (fr'', t_g', _, theta') = msg fr t2 t1
   in if isRenamingOf t1 t2
      then (fr'', t ./ (a |-> Branch (Let theta' t_g') []))
      else (fr', t ./ (a |-> Branch (Let theta t_g) []))

(?) :: Tree -> Node -> Let C
t ? n = treeConf $ sub t n

(??) :: Tree -> Node -> Term C
t ?? n = let (Let _ term) = treeConf $ sub t n in term