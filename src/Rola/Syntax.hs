module Rola.Syntax where

import Rola.Pretty

data Lit = LInt Int
         | LBool Bool
         deriving (Eq, Show)

instance Pretty Lit where
  prettify (LInt i) = show i
  prettify (LBool True) = "true"
  prettify (LBool False) = "false"

type Name = String

data Expr = Var Name
          | Abs Name Expr -- abstraction
          | App Expr Expr -- application
          | Literal Lit
          deriving (Eq, Show)

-- λ
instance Pretty Expr where
  prettify (Var var) = var
  prettify (Abs head body) = "(λ" ++ head ++ "." ++ prettify body ++ ")"
  prettify (App expr expr') = prettify expr ++ " " ++ prettify expr'
  prettify (Literal lit) = prettify lit

i = Abs "x" (Var "x")
id' = App i (Literal (LInt 3))
