{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Language where

import Data.List (intersperse)

newtype Fname = F String
  deriving (Show, Eq)
newtype Gname = G String
  deriving (Show, Eq)

data P -- Program domain
data C -- Abstract value domain
data D -- Ground value domain

-- Only expressions in the program domain and abstract value domain can contain variables
class VarExp d
instance VarExp P
instance VarExp C

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

data Aexp d where
    AtomA :: String -> Aexp d
    VarA :: (VarExp d) => XA d -> Aexp d

data Var d = XA' (XA d)
           | XE' (XE d)

newtype XA d = XA String
newtype XE d = XE String

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