module Trace where

import Language
import Tree
import HomEm

ppt :: Program -> Class -> Tree
ppt prog (ds, _) = trace pm root
    where root = Branch (Let [] t0) []
          t0 = FAppT (F"main") ds
          pm = mkProgMap prog

trace :: ProgMap -> Tree -> Tree
trace pm t = case unproc of
              [] -> t
              (n:_) -> trace pm $ process pm t n
  where unproc = filter (not . isProcessed t) $ leaf t

process :: ProgMap -> Tree -> Node -> Tree
process pm tree beta =
    case filter hem $ relanc tree beta of
      [] -> drive pm tree beta
      alpha:_ -> let at = tree ?? alpha
                 in if at <<= t
                    then abstract tree beta alpha
                    else if t <-> at
                         then drive pm tree beta
                         else abstract tree alpha beta
  where t = tree ?? beta
        hem an = tree ?? an <| t

relanc :: Tree -> Node -> [Node]
relanc tree n = if isProper (tree ? n) then [] else filter isRelevant $ anc n
  where isRelevant an = not $ isProperN tree an

drive :: ProgMap -> Tree -> Node -> Tree
drive = undefined