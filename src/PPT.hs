{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PPT where

import "mtl" Control.Monad.State
import "mtl" Control.Monad.List
import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S

import Language
import Domains

instance Applicative (State s) where
    pure = return
    (<*>) = ap

branch :: (Monad m) => [ListT m a] -> ListT m a
branch = join . ListT . return

(|->) = M.insert

data PPTstate =
  PS { freshEvars :: [CEvar]
     , freshAvars :: [CAvar]
     }

initPPTstate =
  PS { freshEvars = [ CEvar $ "$E" ++ show n | n <- [1..] ]
     , freshAvars = [ CAvar $ "$A" ++ show n | n <- [1..] ]
     }

newtype PPT a = PPT { runPPT :: State PPTstate a }
  deriving (Monad, Functor, Applicative)

class GetFresh a where getFresh :: PPT a

instance GetFresh CEvar where
  getFresh = PPT $ do
    v <- head <$> gets freshEvars
    modify $ \ps -> ps { freshEvars = tail $ freshEvars ps }
    return v

instance GetFresh CAvar where
  getFresh = PPT $ do
    v <- head <$> gets freshAvars
    modify $ \ps -> ps { freshAvars = tail $ freshAvars ps }
    return v

data Trace = Halt Conf
           | Step Conf [(Contr, Trace)]

ppt :: Program -> Class -> Trace
ppt p cls = evalState (runPPT $ trace pm c0) initPPTstate
  where c0 = initConf p cls
        pm = mkProgMap p

trace :: ProgMap -> Conf -> PPT Trace
trace pm conf@((PexpT _, _), _) = return $ Halt conf

trace pm conf@((CallT f es, env), r) = do
  (DefD _ vs t) <- case M.lookup f pm of
                     Nothing -> error $ "Undeclared function called: " ++ show f
                     Just d -> return d
  let es' = map (./ env) es
  let env' = M.fromList $ zipWith (,) vs es'
  let conf' = ((t, env'), r)
  subtrace <- trace pm conf'
  return $ Step conf [(k_id, subtrace)]

trace pm conf@((IfT cond t1 t2, env), r) = do
  brs <- runListT $ (do (s', k) <- traceCond env cond t1 t2
                        let conf'@(_, r') = (s', r) ./ k
                        when (S.member Contra r') mzero
                        subtrace <- lift $ trace pm conf'
                        return (k, subtrace))
  return $ Step conf brs

traceCond :: PCenv -> Cond -> Term -> Term -> ListT PPT (PCstate, Contr)
traceCond env (EqaK ea1 ea2) t1 t2 =
  if ea1' == ea2'
  then return ((t1, env), k_id)
  else branch [brTrue, brFalse]
    where
      (ea1', ea2') = (ea1./env, ea2./env)
      brFalse = return ((t2, env), Right . S.singleton $ ea1' :#: ea2')
      brTrue = do
        when (isTauto $ ea1' :#: ea2') mzero
        let k = Left $ case (ea1', ea2') of
                         (VarCA v, _      ) -> M.singleton (CAvar' v) (AtomC ea2')
                         (_      , VarCA v) -> M.singleton (CAvar' v) (AtomC ea1')
        return ((t1, env), k)

traceCond env (ConsK e xe1 xe2 xa) t1 t2 =
  case e' of
    (ConsC c1 c2) -> let env' = PEvar' xe1 |-> c1
                              $ PEvar' xe2 |-> c2
                              $ env
                     in return ((t1, env'), k_id)
    (AtomC ca) -> let env' = PAvar' xa |-> e' $ env
                  in return ((t2, env'), k_id)
    (VarC xc) -> branch [brTrue xc, brFalse xc]
  where e' = e ./ env
        brTrue xc = do
          x1' <- VarC . CAvar' <$> lift getFresh
          x2' <- VarC . CAvar' <$> lift getFresh
          let env' = PEvar' xe1 |-> x1'
                   $ PEvar' xe2 |-> x2'
                   $ env
          let k = Left $ M.singleton (CEvar' xc) (ConsC x1' x2')
          return ((t1, env'), k)
        brFalse xc = undefined