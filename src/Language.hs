module Language where

import Data.Set (Set, singleton, union, unions, (\\))
import qualified Data.Set as S

newtype XName = X String
  deriving (Eq, Ord, Show)
newtype CName = C String
  deriving (Eq, Ord, Show)
newtype FName = F String
  deriving (Eq, Ord, Show)
newtype Index = I Integer
  deriving (Eq, Ord, Show)

newtype Program = Program [Definition]

data Definition = FunD FName [XName] Expression
  deriving (Show)

data Expression = VarE XName
                | ConsE CName [Expression]
                | ProjE CName Index Expression
                | AppE FName [Expression]
                | IfE Conditional Expression Expression

  deriving (Show)

data Conditional = EqC Expression Expression
                 | ConsC Expression CName
  deriving (Show)

class FreeVars a where freeVars :: a -> Set XName

instance FreeVars Expression where
  freeVars e =
    case e of
      (VarE x) -> singleton x
      (ConsE _ es) -> unions $ map freeVars es
      (AppE _ es) -> unions $ map freeVars es
      (IfE c el er) -> freeVars c `union` (unions $ map freeVars [el,er])

instance FreeVars Conditional where
  freeVars c =
    case c of
      (EqC e1 e2) -> freeVars e1 `union` freeVars e2
      (ConsC e _) -> freeVars e

instance FreeVars Definition where
  freeVars d =
    case d of
      (FunD _ xs e) -> freeVars e \\ S.fromList xs