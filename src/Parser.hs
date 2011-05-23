module Parser where

import Language

import Text.Parsec hiding (optional)
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P

import Data.Char (isLower, isUpper)

import Control.Applicative hiding ((<|>), many)

lDef = haskellStyle
        { P.reservedNames = ["if","then","else"]
        , P.reservedOpNames = [":","="]
        }

lexer = P.makeTokenParser lDef

comma = P.comma lexer
parens = P.parens lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
reservedOp = P.reservedOp lexer
natural = P.natural lexer
dot = P.dot lexer

lowerIdent :: Parser String
lowerIdent = try $ do
  x <- identifier
  if isLower $ head x
     then return x
     else unexpected $ "uppercase identifier " ++ x

upperIdent :: Parser String
upperIdent = try $ do
  x <- identifier
  if isUpper $ head x
    then return x
    else unexpected $ "lowercase identifier " ++ x

xname :: Parser XName
xname = X <$> lowerIdent <?> "variable"

cname :: Parser CName
cname = C <$> upperIdent <?> "constructor"

fname :: Parser FName
fname = F <$> lowerIdent <?> "function name"

index :: Parser Index
index = I <$> natural

program :: Parser Program
program = Program <$> many1 definition <* eof

definition :: Parser Definition
definition = FunD <$> fname <*> parens (xname `sepBy` comma) <* symbol "=" <*> expression

expression :: Parser Expression
expression =   app
           <|> var
           <|> cons
           <|> proj
           <|> if_
           <|> parens expression
  where
    app = try (AppE <$> fname <*> parens exprList)
    var = VarE <$> xname
    cons = ConsE <$> cname <*> parens exprList
    if_ = IfE <$ reserved "if" <*> conditional
              <* reserved "then" <*> expression
              <* reserved "else" <*> expression
    proj = ProjE <$ char '#' <*> cname <* dot <*> index <*> parens expression

    conditional = do
      e <- expression
      ((ConsC e <$ reservedOp ":" <*> cname) <|> (EqC e <$ reservedOp "=" <*> expression))
    exprList  = expression `sepBy` comma