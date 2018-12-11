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

parseInt :: Parser Expr
parseInt = Literal <$> LInt <$> decimal

parseBool :: Parser Expr
parseBool = do
  b <- string "true" <|> string "false"
  pure $
    case b of
      "true"  -> Literal (LBool True)
      "false" -> Literal (LBool False)

parseVar :: Parser Expr
parseVar = Var <$> identifier

parseAbs :: Parser Expr
parseAbs = do
  symbolic 'λ' <|> symbolic '\\'
  args <- some identifier
  symbolic '.'
  body <- parseExpr
  pure (foldr Abs body args)

parseTerm :: Parser Expr
parseTerm =  (parens parseExpr <?> "expression")
         <|> (parseBool <?> "bool")
         <|> (parseVar <?> "identifier")
         <|> (parseInt <?> "number")
         <|> (parseAbs <?> "function")

parseExpr :: Parser Expr
parseExpr = some parseTerm >>= pure . foldl1' App

readExpr :: String -> Either (ParseErrorBundle String Void) Expr
readExpr = parse parseExpr "(input)"

eval :: String -> IO ()
eval expr =
  case readExpr expr of
    Right res -> prettyPrint res
    Left err -> putStr $ errorBundlePretty err
