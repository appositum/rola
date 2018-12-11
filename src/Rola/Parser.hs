module Rola.Parser where

import Data.Void (Void)
import Data.List (foldl1')
import Rola.Syntax
import Rola.Pretty
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

symbolic :: Char -> Parser Char
symbolic = between space space . char

parens :: Parser a -> Parser a
parens = between (symbolic '(') (symbolic ')')

identifier :: Parser String
identifier = do
  x <- letterChar
  xs <- many alphaNumChar
  pure (x:xs)

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
  head <- variable
  symbolic '.'
  body <- parseExpr
  pure (Abs head body)

parseTerm :: Parser Expr
parseTerm =  (parens parseExpr <?> "expression")
         <|> (abstraction      <?> "function")
         <|> (literalBool      <?> "boolean")
         <|> (variable         <?> "identifier")
         <|> (literalInt       <?> "number")

parseExpr :: Parser Expr
parseExpr = some parseTerm >>= pure . foldl1' App

readExpr :: String -> Either (ParseErrorBundle String Void) Expr
readExpr = parse parseExpr "(input)"

eval :: String -> IO ()
eval expr =
  case readExpr expr of
    Right res -> prettyPrint res
    Left err -> putStr $ errorBundlePretty err
