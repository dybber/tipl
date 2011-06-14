{-# LANGUAGE PackageImports #-}

module Print(toGraph, Gr, graphviz', printGraph, printNFA) where

import "mtl" Control.Monad.State

import Tree
import Language
import MSG
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graphviz
import Data.List (intersperse)

getid :: State Int Int
getid = do s <- get
           put $ s + 1
           return s

data EdgeLabel = Contr' Contr
               | Unify [(Var C, Exp C)]

instance Show EdgeLabel where
  show (Contr' (Left Contra)) = "_|_"
  show (Contr' (Left (a:#:b))) = show a ++ " # " ++ show b
  show (Contr' (Right bs)) = concat $ intersperse ", " $ map showB bs
    where showB (a,b) = show a ++ " |-> " ++ show b
  show (Unify us) = concat $ intersperse ", \\n" $ map showu us
    where showu (a,b) = show b ++ " =? [[ " ++ show a ++ " ]]"

toGraph :: Tree -> Gr (Conf) EdgeLabel
toGraph tr = let (ns, es) = evalState (mkNode [] tr) 0
              in mkGraph ns es
  where mkNode anc' (Branch c brs) =
            do ident <- getid
               (nss, ess) <- unzip `fmap` mapM (mkEdge ((ident,c):anc') ident) brs
               let be = findBackEdge ident c anc'
               return ((ident, c):concat nss, be ++ concat ess)
        mkEdge anc'@((_, (Let us _)):_) ident (k, br) =
            do ((n@(nid, _)):ns, es) <- mkNode anc' br
               case us of
                 [] -> return (n:ns, (ident, nid, Contr' k):es)
                 _  -> return (n:ns, (ident, nid, Unify us):es)
        findBackEdge ident c anc' = case filter (isR c) anc' of
                                      [] -> []
                                      ((ident', _):_) -> [(ident, ident', Contr' $ Right [])]
        isR (Let (_:_) _) _ = False
        isR _ (_, Let (_:_) _) = False
        isR (Let [] t1) (_, Let [] t2) = isRenamingOf t1 t2

printGraph :: Tree -> String
printGraph tree = graphviz (toGraph tree) "gr" (8.5, 11) (1, 1) Portrait

printNFA :: Tree -> String
printNFA tree = graphviz (flipGraph $ toGraph tree) "gr" (8.5, 11) (1, 1) Portrait

flipGraph :: Gr Conf EdgeLabel -> Gr (Conf') EdgeLabel
flipGraph gr = nmap terminal (grev gr)
  where terminal c@(Let [] (ExpT _)) = Term c
        terminal _ = Blank

data Conf' = Term Conf
           | Blank

instance Show Conf' where
  show (Term c) = show c
  show Blank = ""