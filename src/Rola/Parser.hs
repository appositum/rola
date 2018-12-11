module Rola.Parser where

import Data.Void (Void)
import Rola.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

symbolic :: Char -> Parser Char
symbolic c = space *> char c <* space

parens :: Parser a -> Parser a
parens = between (symbolic '(') (symbolic ')')

identifier :: Parser String
identifier = do
  x <- letterChar
  xs <- many alphaNumChar
  pure (x:xs)

parseInt :: Parser Expr
parseInt = Literal <$> LInt <$> decimal

parseBool :: Parser Expr
parseBool = do
  b <- string "true" <|> string "false"
  pure $
    case b of
      "true" -> Literal (LBool True)
      "false" -> Literal (LBool False)

parseVar :: Parser Expr
parseVar = Var <$> identifier

parseAbs :: Parser Expr
parseAbs = parens $ do
  symbolic 'λ' <|> symbolic '\\'
  arg <- identifier
  symbolic '.'
  body <- parseExpr
  pure (Abs arg body)

parseApp :: Parser Expr
parseApp = do
  abs <- parseAbs
  expr <- parseExpr
  pure (App abs expr)

parseExpr :: Parser Expr
parseExpr = try (parseApp <?> "lambda application")
         <|> (parseAbs <?> "lambda abstraction")
         <|> (parseInt <?> "number")
         <|> (parseBool <?> "bool")
         <|> (parseVar <?> "identifier")
