{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}

module Domains where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

import "mtl" Control.Monad.Error
import Control.Applicative

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
  deriving (Eq, Ord, Show)

data CAexp = AtomCA String
           | VarCA CAvar
  deriving (Eq, Ord, Show)


data Cvar = CEvar' CEvar
          | CAvar' CAvar
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

data CAvar = CAvar String
  deriving (Eq, Ord, Show)


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
  deriving (Eq, Ord)

isTauto ((AtomCA a1) :#: (AtomCA a2)) = a1 /= a2
isTauto _ = False

isContra Contra = True
isContra (da1 :#: da2) = da1 == da2


---------------
-- Substitution
---------------
type CCsub = Map Cvar Cexp

class Subst a where
  subst :: (Error e, MonadError e m) => CCsub -> a -> m a

instance Subst Restr where
  subst theta r = do
    r' <- S.fromList `liftM` (mapM (subst theta) $ S.elems r)
    if S.fold ((||) . isContra) False r'
      then return $ S.singleton Contra
      else return $ S.filter (not . isTauto) r'

instance Subst Ineq where
  subst _ Contra = return Contra
  subst theta (da1 :#: da2) = (:#:) `liftM` subst theta da1 `ap` subst theta da2

instance Subst Cexp where
  subst theta (ConsC e1 e2) = ConsC `liftM` subst theta e1 `ap` subst theta e2
  subst theta e@(VarC v) = return $ maybe e id $ M.lookup (CEvar' v) theta
  subst theta (AtomC a) = AtomC `liftM` subst theta a

instance Subst CAexp where
  subst theta (AtomCA z) = return $ AtomCA z
  subst theta e@(VarCA cavar) =
    case e' of
      AtomC caexp -> return $ caexp
      _ -> throwError . strMsg $ "Type error: Expected atom but got expression\
                                 \ when looking up variable '" ++ show cavar ++ "'"
    where e' = maybe (AtomC e) id $ M.lookup (CAvar' cavar) theta

instance Subst CRpair where
  subst theta (cexpr, restr) = (,) `liftM` subst theta cexpr `ap` subst theta restr


isFullSubst :: CRpair -> CCsub -> Bool
isFullSubst (cc, r) theta = (all ground $ M.elems theta)
                            && var cc `S.isSubsetOf` (S.fromList $ M.keys theta)

-----------------
-- Concretization
-----------------
toDval :: (Error e, MonadError e m) => Cexp -> m Dval
toDval (ConsC e1 e2) = ConsD `liftM` toDval e1 `ap` toDval e2
toDval (AtomC (AtomCA a)) = return . AtomD . DAtom $ a
toDval _ = throwError . strMsg $ "Illegal attempt to convert non-ground\
                                 \ C-Expression to Dval"

concretize :: CRpair -> [Dval]
concretize = undefined

---------------
-- Contractions
---------------
type Contr = Either CCsub Restr

contract :: (Error e, MonadError e m) => Contr -> CRpair -> m CRpair
contract (Left ccsub) cr = subst ccsub cr
contract (Right kappa) (cc, r) = return (cc, S.union r kappa)

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