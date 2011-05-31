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

---------------
-- Substitution
---------------
type Theta = Map Cvar Cexp

class Subst a where
  subst :: (MonadError String m) => Theta -> a -> m a

instance Subst Restr where
  subst theta r = do
    r' <- S.fromList `liftM` (mapM (subst theta) $ S.elems r)
    if S.fold ((||) . isContra) False r'
      then return $ S.singleton Contra
      else return $ S.filter isTauto r'

    where
      isTauto ((AtomCA a1) :#: (AtomCA a2)) = a1 /= a2
      isTauto _ = False

      isContra Contra = True
      isContra (da1 :#: da2) = da1 == da2


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
      _ -> throwError $ "Type error: Expected atom but got expression\
                        \ when looking up variable '" ++ show cavar ++ "'"
    where e' = maybe (AtomC e) id $ M.lookup (CAvar' cavar) theta

instance Subst CRpair where
  subst theta (cexpr, restr) = (,) `liftM` subst theta cexpr `ap` subst theta restr


---------------
-- Contractions
---------------
type Kappa = Either Theta Restr


------------------
-- Pretty printing
------------------
instance Show Dval where
  show (ConsD e1 e2) = "(cons " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (AtomD (DAtom s)) = "'" ++ s