module Test where

import Domains
import Language
import Parser
import PPT

import qualified Data.Set as S

class Encode a where encode :: a -> Cexp

instance Encode Cexp where encode = id

instance (Encode a) => Encode [a] where
    encode [] = AtomC $ AtomCA $ "nil"
    encode (x:xs) = ConsC (encode x) (encode xs)

instance (Encode a, Encode b) => Encode (Either a b) where
    encode (Left a) = encode a
    encode (Right b) = encode b

xe = Left . VarC . CEvar
xa = Left . AtomC . VarCA . CAvar
val = Right

input :: [Either Cexp Cexp]
input = [xe "in1"]

cls :: Class
cls = (map encode input, S.empty)

test n = do
  eprog <- parseProgramFile "../examples/crap.tsg"
  case eprog of
    Left err -> print err
    Right prog -> let t = tab (ppt prog cls) cls
                      cs = contrs (ppt prog cls)
                      ss = states (ppt prog cls)
                   in mapM_ print $ take n ss