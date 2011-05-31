{-# LANGUAGE FlexibleContexts #-}

module Interp where

import Language

import Control.Monad.Error

import Data.Map (Map)






interpIf :: (MonadError SubstError m) => PDenv -> Cond -> Term -> Term -> m (Term, PDenv)
interpIf env (EqaK ea1 ea2) t1 t2 = do
  ea1' <- subst 