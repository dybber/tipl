{-# LANGUAGE TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}
module Interp where

import Language
import Parser

import qualified Data.Map as M
import Data.Maybe

type Theta = M.Map (Var P) (Exp D)

instance Subst (Term P) Theta (Term D) where
  (FAppT name es) ./ env = FAppT name $ map (./env) es
  (GAppT name es) ./ env = GAppT name $ map (./env) es
  (IfT aexp1 aexp2 t1 t2) ./ env = IfT (aexp1 ./ env) (aexp2 ./ env) (t1 ./ env) (t2 ./ env)
  (ExpT e) ./ env = ExpT (e ./ env)

instance Subst (Exp P) Theta (Exp D) where
  (ConsE e1 e2) ./ env = ConsE (e1./env) (e2./env)
  (VarE xe) ./ env = fromMaybe (error $ "unknown variable " ++ show xe)
                               (M.lookup (XE' xe) env)
  (Aexp' aexp) ./ env = Aexp' $ aexp ./ env

instance Subst (Aexp P) Theta (Aexp D) where
  (AtomA a) ./ _ = AtomA a
  (VarA xa) ./ env = case sub of
                       Aexp' aexp' -> aexp'
                       _ -> error $ "type error: " ++ show xa ++ " was bound to expresion"
      where sub = fromMaybe
                    (error $ "unknown variable " ++ show xa)
                    (M.lookup (XA' xa) env)

getDef :: ProgMap -> FunName -> Definition
getDef pm fn = fromMaybe (error $ "unknown function " ++ show fn)
                         (M.lookup fn pm)

interp :: Program -> [Exp D] -> Maybe Integer -> Term D
interp p ds steps = interp' t0 steps
    where t0 = FAppT (F"main") ds
          progMap = mkProgMap p
          interp' t@(ExpT _) _ = t
          interp' t (Just 0) = t
          interp' t s = interp' (step progMap t) (pred `fmap` s)

step :: ProgMap -> Term D -> Term D
step pm (FAppT fn es) = t ./ theta
  where (FFunD _ vs t) = getDef pm (Fname' fn)
        theta = M.fromList $ zip vs es

step pm (GAppT fn (e:es)) = t' ./ theta
  where (GFunD _ (xe1, xe2, vs1, t1)
                 (xa, vs2, t2)) = getDef pm (Gname' fn)
        theta = M.fromList ms
        (ms, t') = case e of
                     (ConsE e1' e2') -> ([(XE' xe1, e1'), (XE' xe2, e2')]
                                           ++ zip vs1 es, t1)
                     ae@(Aexp' _)    -> ((XA' xa, ae): zip vs2 es, t2)
                     _               -> error "impossible"

step _ (GAppT fn []) = error $ "Application of G-function " ++ show fn ++ " with no arguments"

step _ (IfT ae1 ae2 t1 t2) = if ae1 == ae2 then t1 else t2
step _  e@(ExpT _) = e

interpProg :: String -> [Exp D] -> Maybe Integer -> IO (Term D)
interpProg path as steps = do
  ep <- parseProgramFile path
  case ep of
    Left err -> error $ show err
    Right p -> return $ interp p as steps

class Encode a where enc :: a -> Exp D

instance Encode Char where enc c = Aexp' . AtomA $ [c]
instance (Encode a) => Encode [a] where
    enc [] = Aexp' . AtomA $ "nil"
    enc (x:xs) = ConsE (enc x) (enc xs)

test n = interpProg "examples/findrep.tsg" [enc ['a','b'], enc ['a','c','b','x']] (Just n)