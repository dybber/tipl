{-# LANGUAGE
    PackageImports
  , FlexibleContexts
  , TypeSynonymInstances
  , MultiParamTypeClasses
  #-}
module Language where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Domains


newtype Fname = F String
  deriving (Show, Eq, Ord)

----------------
-- Program terms
----------------
newtype Program = Prog [Definition]
  deriving (Show)

data Definition = DefD Fname [Pvar] Term
  deriving (Show)

data Term = CallT Fname [Pexp]
          | IfT Cond Term Term
          | PexpT Pexp
  deriving (Show)

data Cond = EqaK PAexp PAexp
          | ConsK Pexp PEvar PEvar PAvar
  deriving (Show)

----------------
-- P-Expressions
----------------
data Pexp = ConsP Pexp Pexp
          | VarP PEvar
          | AtomP PAexp
  deriving (Show)

data PAexp = AtomPA String
           | VarPA PAvar
  deriving (Show)

data Pvar = PEvar' PEvar
          | PAvar' PAvar
  deriving (Eq, Ord, Show)


--------------------
-- Typed P-Variables
--------------------
data PEvar = PEvar String
  deriving (Eq, Ord, Show)
data PAvar = PAvar String
  deriving (Eq, Ord, Show)

------------------
-- Definitions map
------------------
type ProgMap = Map Fname Definition

mkProgMap :: Program -> ProgMap
mkProgMap (Prog defs) = foldr ($) M.empty $ map mkInsert defs
  where mkInsert def@(DefD f _ _) = M.insert f def

----------------------
-- Program environment
----------------------
type PDenv = Map Pvar Dval

----------------
-- Program state
----------------
type PDstate = (Term, PDenv)

------------------
-- Program C-state
------------------
type PCenv = Map Pvar Cexp
type PCstate = (Term, PCenv)
type Class = ([Cexp], Restr)
type Conf = (PCstate, Restr)

initConf :: Program -> Class -> Conf
initConf (Prog defs) cls@(ds, r) = ((t_0, env_0), r)
  where
    freshVars = take (length ds) $ [PEvar $ "x" ++ show n | n <- [1..]]
    t_0 = CallT (F"main") (map VarP freshVars)
    env_0 = foldr ($) M.empty $ zipWith (M.insert) (map PEvar' freshVars) ds

instance Subst Class Contr Class where
  (ds, r) ./ (Left sub) = (map (./ sub) ds, r ./ sub)
  (ds, r) ./ (Right r') = (ds, S.union r r')

-------------------------
-- Program C-substitution
-------------------------
instance Subst Pexp PCenv Cexp where
    (./) (ConsP e1 e2) env = ConsC (e1 ./ env) (e2 ./ env)
    (./) (VarP v) env = case M.lookup (PEvar' v) env of
      Nothing -> error $ "Unbound variable " ++ show v
      Just ce -> ce
    (./) (AtomP paexp) env = AtomC (paexp ./ env)

instance Subst PAexp PCenv CAexp where
    (./) (AtomPA syms) env = AtomCA syms
    (./) (VarPA v) env =
      case M.lookup (PAvar' v) env of
        Nothing -> error $ "Unbound variable " ++ show v
        Just (AtomC ca) -> ca
        Just _ -> error $ "Variable is of illegal type: " ++ show v

instance Subst Conf Contr Conf where
    (s, r) ./ (Left ccsub) = (s ./ ccsub, r ./ ccsub)
    (s, r) ./ (Right r') = (s, r `S.union` r')

instance Subst PCstate CCsub PCstate where
    (t, env) ./ sub = (t, env ./ sub)

instance Subst PCenv CCsub PCenv where
    env ./ sub = M.map (./sub) env

instance Subst Term PCenv Cexp where
    (PexpT pexp) ./ env = pexp ./ env

-------------------------------------------
-- Full substitution on program expressions
-------------------------------------------
instance Subst Pexp PDenv Dval where
    (ConsP e1 e2) ./ env = ConsD (e1./env) (e2./env)
    (AtomP paexp) ./ env = AtomD $ paexp ./ env
    (VarP v) ./ env =
        case M.lookup (PEvar' v) env of
          Nothing -> error $ "Unbound expression variable '" ++ show v ++ "'"
          Just e  -> e

instance Subst PAexp PDenv DAval where
    (AtomPA a) ./ env = DAtom a
    (VarPA pavar) ./ env =
        case M.lookup (PAvar' pavar) env of
          Nothing -> error $ "Unbound atom variable '" ++ show pavar ++ "'"
          Just (AtomD daval) -> daval
          Just _ -> error $ "Type Error: Expected atom but got\
                            \ complex value when looking\
                            \ up variable '" ++ show pavar ++ "'"