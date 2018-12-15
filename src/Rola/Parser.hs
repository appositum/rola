module Rola.Parser
  ( parseExpr
  , readExpr
  , Parser(..)
  ) where

import Data.List (foldl1')
import Data.Void (Void)

import Rola.Pretty
import Rola.Syntax

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

symbolic :: Char -> Parser Char
symbolic = between space space . char

parens :: Parser a -> Parser a
parens = between (symbolic '(') (symbolic ')')

identifier :: Parser Name
identifier = (:) <$> letterChar <*> many alphaNumChar

literalInt :: Parser Expr
literalInt = Literal <$> LInt <$> decimal

literalBool :: Parser Expr
literalBool = do
  b <- string "true" <|> string "false"
  pure $
    case b of
      "true"  -> Literal (LBool True)
      "false" -> Literal (LBool False)

variable :: Parser Expr
variable = Var <$> identifier

abstraction :: Parser Expr
abstraction = do
  symbolic 'λ' <|> symbolic '\\'
  arg <- identifier
  symbolic '.'
  expr <- parseExpr
  pure (Lam arg expr)

application :: Parser Expr
application = App <$> abstraction <*> parseExpr

parseTerm :: Parser Expr
parseTerm =  (parens parseExpr <?> "expression")
         <|> (abstraction      <?> "function")
         <|> (literalBool      <?> "boolean")
         <|> (variable         <?> "identifier")
         <|> (literalInt       <?> "number")

parseExpr :: Parser Expr
parseExpr = foldl1' App <$> (parseTerm <?> "term") `sepBy1` space

readExpr :: String -> Either (ParseErrorBundle String Void) Expr
readExpr = parse parseExpr "(input)"
