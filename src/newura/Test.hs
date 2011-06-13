module Test where

import Prelude hiding (exp)
import Language
import Parser
import Trace
import Tree
import Interp
import Print

fromRight :: (Show a) => Either a b -> b
fromRight (Right b) = b
fromRight (Left x) = error $ show x

termP :: String -> Term P
termP = fromRight . parseTerm

expP :: String -> Exp P
expP = fromRight . parseExp

expD :: String -> Exp D
expD str = (expP str) ./ ([] :: [(Var P, Exp D)])

expC :: String -> Exp C
expC = fromRight . parseExp

testRev :: IO Tree
testRev = do
  ep <- parseProgramFile "examples/rev.tsg"
  let clsin = ([expC"X_in"], [])
  case ep of
    Left e -> error $ show e
    Right p -> return $ ppt p clsin

--testFindRep :: IO Tree
--testFindRep = do
--  ep <- parseProgramFile "examples/findrep.tsg"
--  let clsin = ([exp"s", exp"rr"], [])
--  case ep of
--    Left e -> error $ show e
--    Right p -> return $ ppt p clsin

emptyS :: [(Var P, Exp D)]
emptyS = []

testAddInt =
  interpProg "examples/add.tsg" [expD"['S, ['S, 'Z]]", expD"['S, 'Z]"] Nothing

testAddTrace = do
  ep <- parseProgramFile "examples/add.tsg"
  let clsin = ([expC"['S, 'Z]", expC"y"], [])
  case ep of
    Left e -> error $ show e
    Right p -> return $ ppt p clsin

testEq = do
  p <- fromRight `fmap` parseProgramFile "examples/eq.tsg"
  let clsin = ([expC"in"], [])
  return $ ppt p clsin

interpEq n =
  interpProg "examples/eq.tsg" [expD"['A, ['A, 'nil]]"] (Just n)

testPrint :: IO ()
testPrint = do
  tree <- testEq
  putStrLn $ printNFA tree