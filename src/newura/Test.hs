module Main where

import System

import Prelude hiding (exp)
import Language
import Parser
import Trace
--import Tree
--import Interp
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

doGraph :: String -> [Exp C] -> IO ()
doGraph str ds = do
  ep <- parseProgramFile str
  let clsin = (ds, [])
  case ep of
    Left e -> error $ show e
    Right p -> putStrLn $ printGraph $ ppt p clsin

doNfa :: String -> [Exp C] -> IO ()
doNfa str ds = do
  ep <- parseProgramFile str
  let clsin = (ds, [])
  case ep of
    Left e -> error $ show e
    Right p -> putStrLn $ printNFA $ ppt p clsin

main :: IO ()
main = do
  [p] <- getArgs
  doGraph p ins
  doNfa p ins
    where ins = [expC ("Xin" ++ (show v)) | v <- [1..] :: [Integer]]