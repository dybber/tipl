{-# LANGUAGE EmptyDataDecls
  , GADTs
  , MultiParamTypeClasses
  , FunctionalDependencies
  , StandaloneDeriving
  #-}

module Language where

import Data.List (intersperse)
import qualified Data.Map as M

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
class Subst src env dst | src env -> dst where (./) :: src -> env -> dst

----------------------------
-- Convenience abbreviations
----------------------------
type ProgMap = M.Map FunName Definition

-------------------
-- Helper functions
-------------------
mkProgMap :: Program -> ProgMap
mkProgMap (Prog defs) = M.fromList $ map mkPair defs
  where mkPair def@(FFunD fn _ _) = (Fname' fn, def)
        mkPair def@(GFunD fn _ _) = (Gname' fn, def)