module Test where

import Prelude hiding (exp)
import Language
import Parser
import Trace
import Tree

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = undefined

term :: String -> Term C
term = fromRight . parseTerm

exp :: String -> Exp C
exp = fromRight . parseExp

testRev :: IO Tree
testRev = do
  ep <- parseProgramFile "examples/rev.tsg"
  let clsin = ([exp"X_in"], [])
  case ep of
    Left e -> error $ show e
    Right p -> return $ ppt p clsin

testFindRep :: IO Tree
testFindRep = do
  ep <- parseProgramFile "examples/findrep.tsg"
  let clsin = ([exp"s", exp"rr"], [])
  case ep of
    Left e -> error $ show e
    Right p -> return $ ppt p clsin