module Rola.Syntax
  ( Env
  , Expr(..)
  , Literal(..)
  , Name
  ) where

import           Data.Map    (Map)
import           Rola.Pretty

type Name = String
type Env = Map Name Expr

data Literal = LInt Int
             | LBool Bool
             deriving (Eq, Show)

instance Pretty Literal where
  prettify (LInt i)      = show i
  prettify (LBool True)  = "True"
  prettify (LBool False) = "False"

data Expr = Var Name
          | Lam Name Expr     -- λ abstraction
          | Cls Name Expr Env -- closure
          | App Expr Expr     -- application
          | Lit Literal
          deriving (Eq, Show)

instance Pretty Expr where
  prettify (Var var) = var
  prettify (Lit lit) = prettify lit
  prettify (Lam head body) = "(λ" ++ head ++ "." ++ prettify body ++ ")"
  prettify (Cls head body _) = "(λ" ++ head ++ "." ++ prettify body ++ ")"
  prettify (App func expr) = "(" ++ prettify func ++ " " ++ prettify expr ++ ")"
