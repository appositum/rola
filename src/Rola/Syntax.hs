module Rola.Syntax
  ( Env
  , Expr(..)
  , Lit(..)
  , Name
  ) where

import           Data.Map    (Map)
import           Rola.Pretty

type Name = String
type Env = Map Name Expr

data Lit = LInt Int
         | LBool Bool
         deriving (Eq, Show)

instance Pretty Lit where
  prettify (LInt i)      = show i
  prettify (LBool True)  = "true"
  prettify (LBool False) = "false"

data Expr = Var Name
          | Lam Name Expr     -- λ abstraction
          | Cls Name Expr Env -- closure
          | App Expr Expr     -- application
          | Literal Lit
          deriving (Eq, Show)

instance Pretty Expr where
  prettify (Var var) = var
  prettify (Literal lit) = prettify lit
  prettify (Lam head body) = "(λ" ++ head ++ "." ++ prettify body ++ ")"
  prettify (Cls head body _) = "(λ" ++ head ++ "." ++ prettify body ++ ")"
  prettify (App func expr) = "(" ++ prettify func ++ " " ++ prettify expr ++ ")"
