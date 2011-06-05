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

import qualified Debug.Trace as T
import qualified Data.List as L

dbg :: String -> [(String, String)] -> b -> b
dbg title vals = T.trace ("\nDEBUG (" ++ title ++ "):\n" ++ showVals vals)
  where showVals ((k, v):xs) = "\ \ \ \ " ++ k ++ " = " ++ v ++ "\n" ++ showVals xs
        showVals [] = ""

branch :: (Monad m) => [ListT m a] -> ListT m a
branch = join . ListT . return

(|->) :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
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
trace pm conf@((PexpT e, env), r) = return $ Halt conf

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
          x1' <- VarC <$> lift getFresh
          x2' <- VarC <$> lift getFresh
          let env' = PEvar' xe1 |-> x1'
                   $ PEvar' xe2 |-> x2'
                   $ env
          let k = Left $ M.singleton (CEvar' xc) (ConsC x1' x2')
          return $ ((t1, env'), k)
        brFalse xc = do
          x1' <- AtomC . VarCA <$> lift getFresh
          let env' = PAvar' xa |-> x1' $ env
          let k = Left $ M.singleton (CEvar' xc) x1'
          return ((t2, env'), k)

contrs :: Trace -> [Contr]
contrs (Halt _) = []
contrs (Step _ brs) = map fst brs ++ concatMap (contrs . snd) brs

states :: Trace -> [(Integer, Integer, Conf)]
states tr = states' 0 [(0, tr)]
    where states' n ((p, Halt c):xs) = (p,n,c):(states' (n+1) xs)
          states' n ((p, Step c brs):xs) = (p,n,c) : (states' (n+1) $ xs ++ map (\(k,tr) -> (n,tr)) brs)

tab :: Trace -> Class -> [(Class, Cexp)]
tab tr cls = tab' [(cls, tr)]
    where tab' [] = []
          tab' ((cls, Halt ((e, env), _)):xs) = (cls, e ./ env):(tab' xs)
          tab' ((cls, Step _ brs):xs) = tab' $ xs ++ map (\(k,tree) -> (cls ./ k, tree)) brs