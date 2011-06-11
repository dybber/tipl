{-# LANGUAGE
    TypeOperators
  , TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}
module Tree where

import Control.Monad
import Data.List(tails)

import Language
import MSG

data Tree = Branch { treeTerm :: Let C, treeBranches :: [(Contr, Tree)] }

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
sub (Branch _ brs) (ix:ixs) = sub (snd (brs !! ix)) ixs

isProcessed :: Tree -> Node -> Bool
isProcessed tr n = theta == [] && isExpTerm t
                || any (isRenamingOf t) ancTerms
  where Branch (Let theta t) _ = sub tr n
        ancTerms = map letTerm $ filter (not . isProper) $ map (treeTerm . sub tr) $ anc n

isProperN :: Tree -> Node -> Bool
isProperN t n = isProper . treeTerm $ sub t n

abstract :: FreshVars C -> Tree -> Node -> Node -> Maybe (FreshVars C, Tree)
abstract fr t a b = do
  let Branch (Let [] t1) _ = sub t a
  let Branch (Let [] t2) _ = sub t b
  (fr', t_g, theta, _) <- msg fr t1 t2
  when (all (isVar . snd) theta) mzero
  return (fr', Branch (Let theta t_g) [])

(?) :: Tree -> Node -> Let C
t ? n = treeTerm $ sub t n

(??) :: Tree -> Node -> Term C
t ?? n = let (Let _ term) = treeTerm $ sub t n in term