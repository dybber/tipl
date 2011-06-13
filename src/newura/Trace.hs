module Trace(ppt) where

import Language
import Tree
import HomEm
import MSG
import Data.Map ((!))

ppt :: Program -> Class -> Tree
ppt prog (ds, _) = trace pm (initFreshVars, root)
    where root = Branch (Let [] t0) []
          t0 = FAppT (F"main") ds
          pm = mkProgMap prog

trace :: ProgMap -> (FreshVars C, Tree) -> Tree
trace pm (fv, t) = case unproc of
                     [] -> t
                     (n:_) -> trace pm $ process pm fv t n
  where unproc = filter (not . isProcessed t) $ leaf t

process :: ProgMap -> FreshVars C -> Tree -> Node -> (FreshVars C, Tree)
process pm fv tree beta =
    case filter hem $ relanc tree beta of
      [] -> drive pm fv tree beta
      alpha:_ -> let at = tree ?? alpha
                 in if at <<= t
                    then abstract fv tree beta alpha
                    else if t <-> at
                         then drive pm fv tree beta
                         else abstract fv tree alpha beta
  where t = tree ?? beta
        hem an = tree ?? an <| t

relanc :: Tree -> Node -> [Node]
relanc tree n = if isProper (tree ? n) then [] else filter isRelevant $ anc n
  where isRelevant an = not $ isProperN tree an

drive :: ProgMap -> FreshVars C -> Tree -> Node -> (FreshVars C, Tree)
drive pm fv tree n =
    case sub tree n of
      (Branch _ (_:_)) -> error "attempt to drive non-leaf"
      (Branch (Let xs@(_:_) t) []) ->
          (fv, tree ./ (n |-> Branch (Let xs t) [(k_id, Branch (Let [] t) [])]))
      (Branch (Let [] t) []) ->
          let (fv', bs) = driveTerm pm fv t
           in (fv', tree ./ (n |-> Branch (Let [] t) (map mkBranch bs)))
               where mkBranch (k, trm) = (k, Branch (Let [] trm) [])

driveTerm :: ProgMap -> FreshVars C -> Term C -> (FreshVars C, [(Contr, Term C)])
driveTerm pm fv (FAppT fn es) = (fv, [(k_id, t ./ zip vs es)])
  where (FFunD _ vs t) = pm ! Fname' fn

driveTerm pm fv (GAppT fn (e:es)) = (fv3, brTrue ++ brFalse)
  where (GFunD _ (xe1, xe2, vs1, t1) (xa, vs2, t2)) = pm ! Gname' fn
        (xe1', fv1) = getFreshE fv
        (xe2', fv2) = getFreshE fv1
        (xa',  fv3) = getFreshA fv2
        brTrue =
            case e of
              (ConsE e1 e2) ->
                  [( k_id
                   , t1 ./ zip ((XE' xe1):(XE' xe2):vs1) (e1:e2:es))]
              (VarE xe) ->
                  [( Right $ [XE' xe |-> ConsE (varExp xe1') (varExp xe2')]
                   , t1 ./ zip ((XE' xe1):(XE' xe2):vs1)
                               (varExp xe1':varExp xe2':es))]
              (Aexp' _) -> []
        brFalse =
            case e of
              ae@(Aexp' _) ->
                  [( k_id
                   , t2 ./ zip (XA' xa:vs2) (ae:es))]
              (VarE v) ->
                  [( Right $ [XE' v |-> varExp xa']
                   , t2 ./ zip (XA' xa:vs2) (varExp xa':es))]
              (ConsE _ _) -> []

driveTerm _ fv (IfT ea1 ea2 t1 t2) = (fv, brTrue ++ brFalse)
    where brTrue =
              if ea1 == ea2
              then [(k_id, t1)]
              else if isTauto $ ea1 :#: ea2
                   then []
                   else [(mkBind, t1)]
          brFalse = if ea1 /= ea2
                    then [( Left $ ea1:#:ea2, t2)]
                    else []
          mkBind = case (ea1, ea2) of
                             (VarA xa, _) -> Right $ [XA' xa |-> Aexp' ea2]
                             (_, VarA xa) -> Right $ [XA' xa |-> Aexp' ea1]
                             _ -> error "impossible"

driveTerm _ _ (ExpT _) = error "attempt to drive terminating term"
driveTerm _ _ (GAppT _ []) = error "G-application with no arguments"