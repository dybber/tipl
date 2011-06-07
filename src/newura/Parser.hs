{-# LANGUAGE PackageImports #-}
module Parser(prog, parseProgramFile) where

import Text.Parsec hiding (optional)
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import "mtl" Control.Monad.Identity

import Language

lDef :: LanguageDef st
lDef = haskellStyle
        { P.reservedNames = ["if","then","else", "fun"]
        , P.reservedOpNames = ["=", "==", "|", "[", "]"]
        }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser lDef

comma :: Parser String
comma = P.comma lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser String
identifier = P.identifier lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

brackets :: Parser a -> Parser a
brackets = P.brackets lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

dot :: Parser String
dot = P.dot lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

symbols :: Parser String
symbols = lexeme $ many1 (alphaNum <|> oneOf "_-")

fname :: Parser Fname
fname = F <$> identifier

gname :: Parser Gname
gname = G <$> do c <- upper
                 ident <- identifier
                 return (c:ident)

var :: (VarExp d) => Parser (Var d)
var =   (XA' <$> xavar)
    <|> (XE' <$> xevar)

xavar :: Parser (XA d)
xavar = XA <$ dot <*> identifier

xevar :: Parser (XE d)
xevar = XE <$> identifier

prog :: Parser Program
prog = Prog <$> many definition <* eof

definition :: Parser Definition
definition = try gdef <|> fdef

fdef :: Parser Definition
fdef =  FFunD <$ reserved "fun" <*> fname <*> many var <* reservedOp "=" <*> term

gdef :: Parser Definition
gdef = do -- cons
          reserved "fun"
          name1 <- gname
          (x,xs) <- brackets ((,) <$> xevar <* comma <*> xevar)
          vars1 <- many var
          reservedOp "="
          t1 <- term

          -- atom
          reservedOp "|"
          name2 <- gname
          a <- xavar
          vars2 <- many var
          reservedOp "="
          t2 <- term

          when (name1 /= name2) . fail $ "Names " ++ show name1 ++ " " ++ show name2 ++ " does not match"
          when (length vars1 /= length vars2) . fail $ "Length of argument lists for " ++ show name2 ++ "does not match"
          return $ GFunD name1 (x, xs, vars1, t1) (a, vars2, t2)

term :: (VarExp d) => Parser (Term d)
term = parens term <|> callG <|> callF <|> if_ <|> (ExpT <$> pexp)
  where callF = try (FAppT <$> fname <*> many1 pexp)
        callG = try (GAppT <$> gname <*> many1 pexp)
        if_ = IfT <$ reserved "if" <*> aexp <* reservedOp "==" <*> aexp
                  <* reserved "then" <*> term
                  <* reserved "else" <*> term

pexp :: (VarExp d) => Parser (Exp d)
pexp = consE <|> varE <|> (Aexp' <$> aexp)
  where consE = brackets (ConsE <$> pexp <* comma <*> pexp)
        varE = VarE <$> xevar

aexp :: (VarExp d) => Parser (Aexp d)
aexp = atomA <|> varA
  where atomA = AtomA <$ char '\'' <*> symbols
        varA = VarA <$> xavar

parseProgramFile :: String -> IO (Either ParseError Program)
parseProgramFile = parseFromFile (whiteSpace >> prog)
