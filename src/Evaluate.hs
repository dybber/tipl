{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}

module Evaluate where

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

class Decode a where decode :: Dval -> a

instance Decode Integer where
  decode (AtomD (DAtom v)) = read v

instance (Decode a) => Decode [a] where
  decode (ConsD h t) = decode h : decode t
  decode (AtomD (DAtom "nil")) = []
  decode (AtomD (DAtom unk)) = error $ "Unknown symbol '" ++ unk

evalProgramFile :: String -> [Dval] -> IO Dval
evalProgramFile path dvs = do
  res <- parseProgramFile path
  case res of
    Left e -> error $ show e
    Right p -> return $ interp p dvs

evalRev :: [Integer] -> IO [Integer]
evalRev arg = decode `liftM` evalProgramFile "test.tsg" [encode arg]