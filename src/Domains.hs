{-# LANGUAGE
    TypeSynonymInstances
  , MultiParamTypeClasses
  , PackageImports
  , FlexibleContexts
  , FunctionalDependencies
  #-}

module Domains where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad

----------------
-- S-Expressions
----------------
data Dval = ConsD Dval Dval
          | AtomD DAval
  deriving (Eq)

data DAval = DAtom String
  deriving (Eq)

----------------
-- C-Expressions
----------------
data Cexp = ConsC Cexp Cexp
          | VarC CEvar
          | AtomC CAexp
  deriving (Eq, Ord)

data CAexp = AtomCA String
           | VarCA CAvar
  deriving (Eq, Ord)


data Cvar = CEvar' CEvar
          | CAvar' CAvar
  deriving (Eq, Ord)

var :: Cexp -> Set Cvar
var (ConsC e1 e2) = var e1 `S.union` var e2
var (VarC v) = S.singleton $ CEvar' v
var (AtomC (AtomCA _)) = S.empty
var (AtomC (VarCA v)) = S.singleton $ CAvar' v

ground :: Cexp -> Bool
ground ce = var ce == S.empty

--------------------
-- Typed C-variables
--------------------
data CEvar = CEvar String
  deriving (Eq, Ord)

data CAvar = CAvar String
  deriving (Eq, Ord)


---------------------------
-- Restricted C-Expressions
---------------------------
type CRpair = (Cexp, Restr)


---------------
-- Restrictions
---------------
type Restr = Set Ineq


---------------
-- Inequalities
---------------
data Ineq = CAexp :#: CAexp
          | Contra
  deriving (Ord, Show)

instance Eq Ineq where
  (l1 :#: r1) == (l2 :#: r2) =  (l1 == l2) && (r1 == r2)
                             || (l1 == r2) && (r1 == l2)
  Contra == Contra = True
  Contra == _      = False
  _      == Contra = False

isTauto ((AtomCA a1) :#: (AtomCA a2)) = a1 /= a2
isTauto _ = False

isContra Contra = True
isContra (da1 :#: da2) = da1 == da2


---------------
-- Substitution
---------------
type CCsub = Map Cvar Cexp

class Subst src sub dst | src sub -> dst
    where (./) :: src -> sub -> dst
infixl 7 ./

instance Subst Restr CCsub Restr where
  r ./ theta =
    if S.fold ((||) . isContra) False r'
      then S.singleton Contra
      else S.filter (not . isTauto) r'
    where r' = S.fromList (map (./ theta) $ S.elems r)

instance Subst Ineq CCsub Ineq where
    Contra        ./ _     = Contra
    (da1 :#: da2) ./ theta = (da1 ./ theta) :#: (da2 ./ theta)

instance Subst Cexp CCsub Cexp where
    (ConsC e1 e2) ./ theta = ConsC (e1 ./ theta) (e2 ./ theta)
    e@(VarC v)    ./ theta = maybe e id $ M.lookup (CEvar' v) theta
    (AtomC a)     ./ theta = AtomC (a ./ theta)

instance Subst CAexp CCsub CAexp where
    (AtomCA z)      ./ _     = AtomCA z
    e@(VarCA cavar) ./ theta =
        case e' of
          AtomC caexp -> caexp
          _ -> error $ "Type error: Expected atom but got expression\
                       \ when looking up variable '" ++ show cavar ++ "'"
        where e' = maybe (AtomC e) id $ M.lookup (CAvar' cavar) theta

instance Subst CRpair CCsub CRpair where
    (cexpr, restr) ./ theta = (cexpr ./ theta, restr ./ theta)


isFullSubst :: CRpair -> CCsub -> Bool
isFullSubst (cc, r) theta = (all ground $ M.elems theta)
                            && var cc `S.isSubsetOf` (S.fromList $ M.keys theta)

-----------------
-- Concretization
-----------------
toDval :: Cexp -> Dval
toDval (ConsC e1 e2) = ConsD (toDval e1) (toDval e2)
toDval (AtomC (AtomCA a)) = AtomD . DAtom $ a
toDval _ = error $ "Illegal attempt to convert non-ground\
                   \ C-Expression to Dval"

concretize :: CRpair -> [Dval]
concretize = undefined

---------------
-- Contractions
---------------
type Contr = Either CCsub Restr

k_id = Left M.empty
k_contra = Right $ M.singleton Contra

---------
-- Splits
---------
type Split = (Contr, Contr)

------------------
-- Pretty printing
------------------
instance Show Dval where
  show (ConsD e1 e2) = "(cons " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (AtomD (DAtom s)) = "'" ++ s


instance Show Cexp where
  show (ConsC e1 e2) = "(cons " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (VarC cev) = show cev
  show (AtomC a) = show a

instance Show CAexp where
  show (AtomCA s) = "'" ++ s
  show (VarCA v) = show v

instance Show CEvar where
  show (CEvar v) = v

instance Show CAvar where
  show (CAvar v) = "." ++ v