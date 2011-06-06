{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PPT where

import "mtl" Control.Monad.State
import Control.Applicative
import Control.Arrow (first, second)

import qualified Data.Map as M
import qualified Data.Set as S

import Language
import Domains

import qualified Data.List as L

(|->) :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
(|->) = M.insert

type FreshVars = [Integer]
class GetFresh a where getFresh :: FreshVars -> (a, FreshVars)

instance GetFresh CEvar where
  getFresh (x:xs) = (CEvar $ "$E" ++ show x, xs)

instance GetFresh CAvar where
  getFresh (x:xs) = (CAvar $ "$A" ++ show x, xs)

data Trace = Halt Conf
           | Step Conf [(Contr, Trace)]

ppt :: Program -> Class -> Trace
ppt p cls = trace pm [1..] c0
  where c0 = initConf p cls
        pm = mkProgMap p

trace :: ProgMap -> FreshVars -> Conf -> Trace
trace pm fr conf@((PexpT e, env), r) = Halt conf

trace pm fr conf@((CallT f es, env), r) = Step conf [(k_id, trace pm fr conf')]
    where
      (DefD _ vs t) = case M.lookup f pm of
                        Nothing -> error $ "Undeclared function called: " ++ show f
                        Just d -> d
      es' = map (./ env) es
      env' = M.fromList $ zip vs es'
      conf' = ((t, env'), r)

trace pm fr conf@((IfT cond t1 t2, env), r) = Step conf brs
    where brs = do (s', k, fr') <- traceCond env cond t1 t2 fr
                   let conf'@(_, r') = (s', r) ./ k
                   when (S.member Contra r') mzero
                   let subtrace = trace pm fr' conf'
                   return (k, subtrace)

traceCond :: PCenv -> Cond -> Term -> Term -> FreshVars -> [(PCstate, Contr, FreshVars)]
traceCond env (EqaK ea1 ea2) t1 t2 fr =
  if ea1' == ea2'
  then return ((t1, env), k_id, fr)
  else join [brTrue, brFalse]
    where
      (ea1', ea2') = (ea1./env, ea2./env)
      brFalse = return ((t2, env), Right . S.singleton $ ea1' :#: ea2', fr)
      brTrue = do
        when (isTauto $ ea1' :#: ea2') mzero
        let k = Left $ case (ea1', ea2') of
                         (VarCA v, _      ) -> M.singleton (CAvar' v) (AtomC ea2')
                         (_      , VarCA v) -> M.singleton (CAvar' v) (AtomC ea1')
        return ((t1, env), k, fr)

traceCond env (ConsK e xe1 xe2 xa) t1 t2 fr =
  case e' of
    (ConsC c1 c2) -> let env' = PEvar' xe1 |-> c1
                              $ PEvar' xe2 |-> c2
                              $ env
                     in return ((t1, env'), k_id, fr)
    (AtomC ca) -> let env' = PAvar' xa |-> e' $ env
                  in return ((t2, env'), k_id, fr)
    (VarC xc) -> join [brTrue xc, brFalse xc]
  where e' = e ./ env
        brTrue xc = return ((t1, env'), k, fr'')
            where
              (x1', fr') = first VarC $ getFresh fr
              (x2', fr'') = first VarC $ getFresh fr'
              env' = PEvar' xe1 |-> x1'
                   $ PEvar' xe2 |-> x2'
                   $ env
              k = Left $ M.singleton (CEvar' xc) (ConsC x1' x2')

        brFalse xc = return ((t2, env'), k, fr')
            where
              (x1', fr') = first (AtomC . VarCA) $ getFresh fr
              env' = PAvar' xa |-> x1' $ env
              k = Left $ M.singleton (CEvar' xc) x1'

tab :: Trace -> Class -> [(Class, Cexp)]
tab tr cls = tab' [(cls, tr)]
    where tab' [] = []
          tab' ((cls, Halt ((e, env), _)):xs) = (cls, e ./ env):(tab' xs)
          tab' ((cls, Step _ brs):xs) = tab' $ xs ++ map (\(k,tree) -> (cls ./ k, tree)) brs