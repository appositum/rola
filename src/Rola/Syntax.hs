module Rola.Syntax
  ( Expr(..)
  , Lit(..)
  ) where

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
          | Lam Name Expr -- λ abstraction
          | App Expr Expr -- application
          | Literal Lit
          deriving (Eq, Show)

instance Pretty Expr where
  prettify (Var var) = var
  prettify (Literal lit) = prettify lit
  prettify (Lam head body) = "(λ" ++ head ++ "." ++ prettify body ++ ")"
  prettify (App func expr) =  "(" ++ prettify func ++ " " ++ prettify expr ++ ")"
