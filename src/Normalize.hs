{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}

module Normalize where

import "mtl" Control.Monad.Error
import "mtl" Control.Monad.Identity

import Domains
import Interp
import Parser

class Encode a where encode :: a -> Dval

instance (Encode a) => Encode [a] where
  encode [] = AtomD $ DAtom "nil"
  encode (x:xs) = ConsD (encode x) (encode xs)

instance Encode Integer where
  encode = AtomD . DAtom . show

normalizeProgram :: String -> [Dval] -> IO ()
normalizeProgram path dvs = do
  res <- parseProgramFile path
  case res of
    Left e -> putStrLn $ "Parse error: " ++ show e
    Right p -> case runIdentity . runErrorT $ normalize p dvs of
                 Left se -> putStrLn $ "Interpretation error: " ++ show se
                 Right v -> putStrLn $ show v