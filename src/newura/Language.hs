{-# LANGUAGE EmptyDataDecls
  , GADTs
  , MultiParamTypeClasses
  , FunctionalDependencies
  , StandaloneDeriving
  , FlexibleInstances
  , FlexibleContexts
  , TypeOperators
  , TypeSynonymInstances
  #-}

module Language where

import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

newtype Fname = F String
  deriving (Show, Eq, Ord)
newtype Gname = G String
  deriving (Show, Eq, Ord)

data FunName = Fname' Fname | Gname' Gname
  deriving (Eq, Ord, Show)

-- Uninhabited types for denoting different expression domains
data P -- ^Program domain
data C -- ^Abstract value domain
data D -- ^Ground value domain

-- Only expressions in the program domain and abstract value domain can contain variables
class VarExp d
class GroundExp d
instance VarExp P
instance VarExp C
instance GroundExp D

data Program = Prog [Definition]
               deriving Show

data Definition = FFunD Fname [Var P] (Term P)
                | GFunD Gname (XE P, XE P, [Var P], Term P)
                              (XA P, [Var P], Term P)
                  deriving Show

data Term d = FAppT Fname [Exp d]
            | GAppT Gname [Exp d]
            | IfT (Aexp d) (Aexp d) (Term d) (Term d)
            | ExpT (Exp d)

data Exp d where
    ConsE :: Exp d -> Exp d -> Exp d
    VarE  :: (VarExp d) => XE d -> Exp d
    Aexp' :: Aexp d -> Exp d

deriving instance Eq (Exp d)

data Aexp d where
    AtomA :: String -> Aexp d
    VarA :: (VarExp d) => XA d -> Aexp d

deriving instance Eq (Aexp d)

data Var d = XA' (XA d)
           | XE' (XE d)
  deriving (Eq, Ord)

newtype XA d = XA String
  deriving (Eq, Ord)
newtype XE d = XE String
  deriving (Eq, Ord)

isVar :: Exp d -> Bool
isVar (VarE _) = True
isVar (Aexp' (VarA _)) = True
isVar _ = False

isExpTerm :: Term d -> Bool
isExpTerm (ExpT _) = True
isExpTerm _ = False

----------------------------
-- Domain of let-bound terms
----------------------------
data Let d = Let { letTheta :: [(Var d, Exp d)], letTerm :: Term d }

isProper :: Let d -> Bool
isProper (Let [] _) = False
isProper _ = True

-----------------
-- Show instances
-----------------
showSpaced :: (Show a) => [a] -> String
showSpaced xs = concat . intersperse " " $ map show xs

instance Show (Term d) where
  show (FAppT (F f) es) = "(" ++ f ++ " " ++ showSpaced es ++ ")"
  show (GAppT (G g) es) = "(" ++ g ++ " " ++ showSpaced es ++ ")"
  show (IfT xa1 xa2 t1 t2) = "(if " ++ show xa1 ++ " == "
                            ++ show xa2 ++ " then " ++ show t1 ++ " else " ++ show t2 ++ ")"
  show (ExpT e) = show e

instance Show (Exp d) where
  show (ConsE e1 e2) = "[" ++ show e1 ++ ", " ++ show e2 ++ "]"
  show (VarE xe) = show xe
  show (Aexp' e) = show e

instance Show (Aexp d) where
  show (AtomA s) = "'" ++ s
  show (VarA xa) = show xa

instance Show (Var d) where
  show (XA' s) = show s
  show (XE' s) = show s

instance Show (XE d) where show (XE s) = s
instance Show (XA d) where show (XA s) = '.':s

---------------------
-- Substitution class
---------------------
class Subst src env dst | src env -> dst
    where (./) :: src -> env -> dst

-- Instances for partial substitution
instance Subst (Term d) (M.Map (Var d) (Exp d)) (Term d) where
    t ./ env = substT t env id

instance Subst (Exp d) (M.Map (Var d) (Exp d)) (Exp d) where
    e ./ env = substE e env id

instance Subst (Aexp d) (M.Map (Var d) (Exp d)) (Aexp d) where
    ea ./ env = substA ea env id

instance (Subst (x d) (M.Map (Var d) (Exp d)) (x d)) =>
          Subst (x d) [(Var d, Exp d)] (x d) where
    t ./ env = t ./ (M.fromList env)


-- Full substitution on terms
(.//) :: Term t -> M.Map (Var t) (Exp d) -> Term d
t .// env = substT t env (\e -> error $ "Unbound variable: " ++ show e)

-- Renaming of keys in a substitution
instance Subst (Var d :-> Exp d) [Var d :-> Var d] (Var d :-> Exp d) where
  (v, e) ./ vs = case [v'' | (v', v'') <- vs, v' == v] of
                   [v''] -> (v'', e)
                   [] -> (v, e)
                   _ -> error "inconsistent map"


-- Generalized substitution implementation on terms and expressions.
-- The 'f' parameter is used when the given substitution map is only partially
-- defined on the domain of free variables.
substT :: Term t -> M.Map (Var t) (Exp d) -> (Exp t -> Exp d) -> Term d
substT (FAppT name es) env f = FAppT name $ map (\e -> substE e env f) es
substT (GAppT name es) env f = GAppT name $ map (\e -> substE e env f) es
substT (IfT aexp1 aexp2 t1 t2) env f = IfT (substA aexp1 env f)
                                           (substA aexp2 env f)
                                           (substT t1 env f)
                                           (substT t2 env f)
substT (ExpT e) env  f = ExpT (substE e env f)

substE :: Exp t -> M.Map (Var t) (Exp d) -> (Exp t -> Exp d) -> Exp d
substE (ConsE e1 e2) env f = ConsE (substE e1 env f) (substE e2 env f)
substE (VarE xe) env f = fromMaybe (f $ VarE xe)
                                   (M.lookup (XE' xe) env)
substE (Aexp' aexp) env f = Aexp' $ substA aexp env f

substA :: Aexp d1 -> M.Map (Var d1) (Exp d) -> (Exp d1 -> Exp d) -> Aexp d
substA (AtomA a) _ _ = AtomA a
substA (VarA xa) env f = case sub of
                           Aexp' aexp' -> aexp'
                           _ -> error $ "type error: "
        where sub = fromMaybe
                      (f $ Aexp' $ VarA $ xa)
                      (M.lookup (XA' xa) env)

---------------
-- Contractions
---------------
data Ineq = Aexp C :#: Aexp C | Contra
  deriving (Show)

instance Eq Ineq where
  (l1 :#: r1) == (l2 :#: r2) =  (l1 == l2) && (r1 == r2)
                             || (l1 == r2) && (r1 == l2)
  Contra == Contra = True
  Contra == _      = False
  _      == Contra = False

isTauto :: Ineq -> Bool
isTauto ((AtomA a1) :#: (AtomA a2)) = a1 /= a2
isTauto _ = False

isContra :: Ineq -> Bool
isContra Contra = True
isContra (da1 :#: da2) = da1 == da2

type Contr = Either Ineq [Var C :-> Exp C]

k_id :: Contr
k_id = Right []

k_contra :: Contr
k_contra = Left Contra


--------
-- Class
--------
type Class = ([Exp C], [Ineq])

----------------------------
-- Convenience abbreviations
----------------------------
type ProgMap = M.Map FunName Definition

(|->) :: a -> b -> (a, b)
(|->) = (,)

type a :-> b = (a, b)

type Theta d t = M.Map (Var d) (Exp t)
type Theta' d = Theta d d
type ThetaL d t = [(Var d, Exp t)]
type ThetaL' d = ThetaL d d

-----------------------------
-- Helper functions and types
-----------------------------
mkProgMap :: Program -> ProgMap
mkProgMap (Prog defs) = M.fromList $ map mkPair defs
  where mkPair def@(FFunD fn _ _) = (Fname' fn, def)
        mkPair def@(GFunD fn _ _) = (Gname' fn, def)

type FreshVars d = ([XE d], [XA d])

getFreshE :: FreshVars d -> (Var d, FreshVars d)
getFreshE (xe:xes, xas) = (XE' xe, (xes, xas))
getFreshE _ = error "ran out of fresh variables"

getFreshA :: FreshVars d -> (Var d, FreshVars d)
getFreshA (xes, xa:xas) = (XA' xa, (xes, xas))
getFreshA _ = error "ran out of fresh variables"

getFreshesE :: FreshVars d -> Int -> ([Var d], FreshVars d)
getFreshesE (xes, xas) n = (map XE' $ take n xes, (drop n xes, xas))

getFreshesA :: FreshVars d -> Int -> ([Var d], FreshVars d)
getFreshesA (xes, xas) n = (map XA' $ take n xas, (xes, drop n xas))

initFreshVars :: (VarExp d) => FreshVars d
initFreshVars = ( [XE $ "$Xe" ++ show n | n <- ixs1]
                , [XA $ "$Xa" ++ show n | n <- ixs2])
    where ixs1 = [1..] :: [Integer]
          ixs2 = [1..] :: [Integer]

varExp :: (VarExp d) => Var d -> Exp d
varExp (XE' xe) = VarE xe
varExp (XA' xa) = Aexp' . VarA $ xa