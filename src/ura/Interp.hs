{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

module Interp where

import Language
import Domains

import "mtl" Control.Monad.Error
import "mtl" Control.Monad.Trans

import Data.Map (Map)
import qualified Data.Map as M
import Data.Function (fix)

(|->) :: (Ord k) => k -> a -> Map k a -> Map k a
(|->) = M.insert

interpIf :: PDenv -> Cond -> Term -> Term -> PDstate
interpIf env (EqaK ea1 ea2) t1 t2 =
  if (ea1 ./ env == ea2 ./ env)
    then (t1, env)
    else (t2, env)

interpIf env (ConsK pe xe1 xe2 xa) t1 t2 =
  let de = pe ./ env
  in case de of
       (ConsD d1 d2) -> let env' = PEvar' xe1 |-> d1
                                 $ PEvar' xe2 |-> d2 $ env
                        in (t1, env')
       (AtomD _)     -> let env' = PAvar' xa |-> de $ env
                        in (t2, env')

step :: ProgMap -> PDstate -> PDstate
step gamma (IfT k t1 t2, env) = interpIf env k t1 t2
step gamma (CallT f es, env) =
  case M.lookup f gamma of
    Nothing -> error $ "Unknown function '" ++ show f ++ "'"
    Just (DefD _ vs t) ->
      let es' = map (./ env) es
          env' = foldr ($) env $ zipWith (|->) vs es'
       in (t, env')

interp :: Program -> [Dval] -> Dval
interp p ds =
    fix (\f s@(t, env) ->
             case t of
               (PexpT pexp) -> pexp ./ env
               _            -> f (step progMap s)
        ) s_0
  where progMap = mkProgMap p
        freshVars = take (length ds) $ [PEvar $ "x" ++ show n | n <- [1..]]
        mkInitEnv vs ds = foldr ($) M.empty $ zipWith (|->) vs ds
        s_0 = ( CallT (F"main") $ map VarP freshVars
              , mkInitEnv (map PEvar' freshVars) ds)