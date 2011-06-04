module Parser(prog, parseProgramFile) where

import Text.Parsec hiding (optional)
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P

import Data.Char (isLower, isUpper)

import Control.Applicative hiding ((<|>), many)

import Language

lDef = haskellStyle
        { P.reservedNames = ["if","then","else","cons", "case", "of", "fun"]
        , P.reservedOpNames = ["=", "==", "->", "|"]
        }

lexer = P.makeTokenParser lDef

comma = P.comma lexer
parens = P.parens lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
angles = P.angles lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
reservedOp = P.reservedOp lexer
natural = P.natural lexer
dot = P.dot lexer
whiteSpace = P.whiteSpace lexer


symbols = lexeme $ many1 (alphaNum <|> oneOf "_-")

fname :: Parser Fname
fname = F <$> identifier

var :: Parser Pvar
var =   (PAvar' <$> pavar)
    <|> (PEvar' <$> pevar)

pavar = PAvar <$ dot <*> identifier
pevar = PEvar <$> identifier

prog :: Parser Program
prog = Prog <$> many definition <* eof

definition :: Parser Definition
definition = DefD <$ reserved "fun" <*> fname <*> many var <* reservedOp "=" <*> term

term :: Parser Term
term = parens term <|> call <|> if_ <|> case_ <|> (PexpT <$> pexp)
  where call = try (CallT <$> fname <*> many1 pexp)
        if_ = IfT <$> (EqaK <$ reserved "if" <*> paexp <* reservedOp "==" <*> paexp)
                  <*  reserved "then" <*> term
                  <*  reserved "else" <*> term

        case_ = do
          reserved "case"
          e <- pexp
          reserved "of"
          (a,b) <- angles ((,) <$> pevar <* comma <*> pevar)
          reservedOp "->"
          t1 <- term
          reservedOp "|"
          c <- pavar
          reservedOp "->"
          t2 <- term
          return $ IfT (ConsK e a b c) t1 t2

pexp :: Parser Pexp
pexp = consE <|> varE <|> (AtomP <$> paexp)
  where consE = angles (ConsP <$> pexp <* comma <*> pexp)
        varE = VarP <$> pevar

paexp :: Parser PAexp
paexp = atomA <|> varA
  where atomA = AtomPA <$ char '\'' <*> symbols
        varA = VarPA <$> pavar

parseProgramFile :: String -> IO (Either ParseError Program)
parseProgramFile = parseFromFile (whiteSpace >> prog)