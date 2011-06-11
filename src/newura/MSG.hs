{-# LANGUAGE
    TypeOperators
  , FlexibleInstances
  , MultiParamTypeClasses
  , TypeSynonymInstances
  #-}
module MSG (msg, isRenamingOf) where

import Data.List (intersect)
import Data.Maybe (listToMaybe)
import Data.Monoid

import Language

isRenamingOf :: (VarExp d) => Term d -> Term d -> Bool
isRenamingOf t1 t2 =
    case msg initFreshVars t1 t2 of
      Just (_, _, theta1, theta2) ->
           (all (isVar . snd) theta1 && theta2 == [])
        || (all (isVar . snd) theta2 && theta1 == [])
      Nothing -> False

-- Normalization, for easily detecting the case where one term is an instance of the other
normalize :: (VarExp d)
          => (FreshVars d, Term d, ThetaL' d, ThetaL' d)
          -> (FreshVars d, Term d, ThetaL' d, ThetaL' d)
normalize m@(fr, t, theta1, theta2) =
    case getRenaming theta1 of
      Just rn1 -> (fr, t ./ theta1, [], map (./rn1) theta2)
      Nothing ->  case getRenaming theta2 of
                    Just rn2 -> (fr, t./theta2, map (./rn2) theta1, [])
                    Nothing -> m

getRenaming :: (VarExp d) => [Var d :-> Exp d] -> Maybe [Var d :-> Var d]
getRenaming [] = Just []
getRenaming (x:xs) = do sub <- aux x
                        subs <- getRenaming xs
                        return $ sub:subs
 where aux (XE' xe, VarE xe') = Just $ XE' xe |-> XE' xe'
       aux (XA' xa, (Aexp' (VarA xa'))) = Just $ XA' xa |-> XA' xa'
       aux _ = Nothing

msg :: (VarExp d)
    => FreshVars d
    -> Term d
    -> Term d
    -> Maybe (FreshVars d, Term d, ThetaL' d, ThetaL' d)
msg fr t1 t2 = do
  (fr', t, theta1, theta2) <- extractExprs fr t1 t2
  return $ normalize $ msg' (fr', t, theta1, theta2)

extractExprs :: (VarExp d)
             => FreshVars d
             -> Term d
             -> Term d
             -> Maybe (FreshVars d, Term d, ThetaL' d, ThetaL' d)
extractExprs fr (FAppT f es) (FAppT f' es')
    | f /= f' = Nothing
    | otherwise = return (fr', FAppT f ves, theta1, theta2)
    where (vs, fr') = getFreshesE fr $ length es
          ves = map varExp vs
          theta1 = zip vs es
          theta2 = zip vs es'

extractExprs fr (GAppT g es) (GAppT g' es')
    | g /= g' = Nothing
    | otherwise = return (fr', GAppT g ves, theta1, theta2)
    where (vs, fr') = getFreshesE fr $ length es
          ves = map varExp vs
          theta1 = zip vs es
          theta2 = zip vs es'

extractExprs fr (IfT ea1 ea2 t1 t2) (IfT ea1' ea2' t1' t2') = do
  let ([v1@(XA' v1_), v2@(XA' v2_)], fr') = getFreshesA fr 2
  (fr'', t1_, theta1, theta2)    <- extractExprs fr' t1 t1'
  (fr''', t2_, theta1', theta2') <- extractExprs fr'' t2 t2'
  return (fr''', (IfT (VarA v1_) (VarA v2_) t1_ t2_)
               , [v1 |-> Aexp' ea1,  v2 |-> Aexp' ea2 ] ++ theta1 ++ theta1'
               , [v1 |-> Aexp' ea1', v2 |-> Aexp' ea2'] ++ theta2 ++ theta2')

extractExprs fr (ExpT e) (ExpT e') = return (fr', ExpT $ varExp v, [v |-> e], [v |-> e'])
  where (v, fr') = getFreshE fr

extractExprs _ _ _ = Nothing

msg' :: (VarExp d) =>
        (FreshVars d, Term d, ThetaL' d, ThetaL' d)
     -> (FreshVars d, Term d, ThetaL' d, ThetaL' d)
msg' s@(fv, t, theta1, theta2) =
    case dup (t, theta1, theta2) of
      Just (t', theta1', theta2') -> msg' (fv, t', theta1', theta2')
      Nothing -> case common s of
                   Just s' -> msg' s'
                   Nothing -> s

dup :: (VarExp d) => (Term d, ThetaL' d, ThetaL' d) -> Maybe (Term d, ThetaL' d, ThetaL' d)
dup (t, theta1, theta2) = do
  let ds1 = dups theta1
  let ds2 = dups theta2
  (x, y) <- listToMaybe $ intersect ds1 ds2
  -- Make sure that we don't bind an atom variable to an expression variable
  let (x', y') = case (x, y) of
                   ((XA' _), (XE' _)) -> (y, x)
                   (_, _)             -> (x, y)
  let filter' = filter ((/=x') . fst)
  return (t ./ [x' |-> varExp y'], filter' theta1, filter' theta2)
      where dups [] = []
            dups ((k, v):xs) = [ord k k' | (k', v') <- xs, v == v'] ++ dups xs
            ord k1 k2 = if k1 <= k2 then (k1, k2) else (k2, k1)

common :: (VarExp d) =>
          (FreshVars d, Term d, ThetaL' d, ThetaL' d)
       -> Maybe (FreshVars d, Term d, ThetaL' d, ThetaL' d)
common (fr, t, theta1, theta2) = do
  let cmpRes = getFirst . mconcat . map First $ [comp fr b1 b2 | b1 <- theta1, b2 <- theta2]
  (fr', esubst@(x, _), dt1, dt2) <- cmpRes
  let filter' = filter ((/=x) . fst)
  return (fr', t ./ [esubst], dt1 ++ filter' theta1, dt2 ++ filter' theta2)

comp :: (VarExp d) =>
           FreshVars d
        -> (Var d, Exp d)
        -> (Var d, Exp d)
        -> Maybe ( FreshVars d
                 , (Var d, Exp d)
                 , [(Var d, Exp d)]
                 , [(Var d, Exp d)] )

comp _ (x, _) (x', _) | x /= x' = Nothing
comp fr (x, ConsE e1 e2) (_, ConsE e1' e2') = Just ( fr''
                                                   , x |-> commonExp
                                                   , [y1 |-> e1,  y2 |-> e2]
                                                   , [y1 |-> e1', y2 |-> e2']
                                                   )
    where (y1, fr') = getFreshE fr
          (y2, fr'') = getFreshE fr'
          commonExp = ConsE (varExp y1) (varExp y2)

-- Handles both atoms and avars
comp fr (x, (Aexp' a)) (_, Aexp' a')
    | a /= a' = Nothing
    | otherwise = Just (fr, x |-> (Aexp' a), [], [])

comp fr (x, (VarE v)) (_, (VarE v'))
    | v /= v' = Nothing
    | otherwise = Just (fr, x |-> (VarE v), [], [])

comp _ _ _ = Nothing