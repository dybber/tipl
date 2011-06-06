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

instance Encode Char where
    encode c = AtomC $ AtomCA $ c:[]

xe = Left . VarC . CEvar
xa = Left . AtomC . VarCA . CAvar
val = Right

input :: [[Either Cexp Char]]
input = [[xe "e1", xe "e2"], [xe "e1", xe "e3"]]

cls :: Class
cls = (map encode input, S.empty)

test n = do
  eprog <- parseProgramFile "../examples/findrep.tsg"
  case eprog of
    Left err -> putStrLn $ show err
    Right prog -> let t = tab (ppt prog cls) cls
                   in mapM_ (putStrLn . show) $ take n t