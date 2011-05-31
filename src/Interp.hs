{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

module Interp where

import Language
import Domains

import "mtl" Control.Monad.Error
import "mtl" Control.Monad.Trans

import Data.Map (Map)
import qualified Data.Map as M

type ProgMap = Map Fname Definition

(|->) = M.insert

interpIf :: (MonadError String m) => PDenv -> Cond -> Term -> Term -> m (Term, PDenv)
interpIf env (EqaK ea1 ea2) t1 t2 = do
  ed1 <- substEnv env (AtomP ea1)
  ed2 <- substEnv env (AtomP ea2)
  if (ed1 == ed2)
    then return (t1, env)
    else return (t2, env)

interpIf env (ConsK pe xe1 xe2 xa) t1 t2 = do
  de <- substEnv env pe
  case de of
    (ConsD d1 d2) -> let env' = PEvar' xe1 |-> d1
                              $ PEvar' xe2 |-> d2 $ env
                      in return (t1, env')
    (AtomD _)     -> let env' = PAvar' xa |-> de $ env
                      in return (t2, env')

interp :: (MonadError String m) => ProgMap -> Term -> PDenv -> m (Term, PDenv)
interp gamma (IfT k t1 t2) env = interpIf env k t1 t2
interp gamma (CallT f es) env =
  case M.lookup f gamma of
    Nothing -> throwError $ "Unknown function '" ++ show f ++ "'"
    Just (DefD _ vs t) -> do
      es' <- mapM (substEnv env) es
      let env' = foldr ($) env $ zipWith (|->) vs es'
      return (t, env')

normalize :: (MonadError String m) => Program -> [Dval] -> m Dval
normalize (Prog defs) ds =
  case M.lookup (F "main") progMap of
    Nothing -> throwError $ "No main function defined"
    Just (DefD _ vs t) -> normalize' (t, mkInitEnv vs ds)

  where progMap = foldr ($) M.empty $ map mkInsert defs
        mkInsert def@(DefD f _ _) = M.insert f def
        mkInitEnv vs ds = foldr ($) M.empty $ zipWith (|->) vs ds
        normalize' (t, env) = case t of
          (PexpT pexp) -> substEnv env pexp
          _ -> interp progMap t env >>= normalize'