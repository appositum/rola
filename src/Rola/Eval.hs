module Rola.Eval
  ( eval
  , reduce
  ) where

import qualified Data.Map as M
import Rola.Parser
import Rola.Pretty
import Rola.Syntax
import Text.Megaparsec (errorBundlePretty)

type Error = String

quote :: String -> String
quote str = "\"" ++ str ++ "\""

envLookup :: Env -> Name -> Either Error Expr
envLookup env name =
  case M.lookup name env of
    Just expr -> Right expr
    Nothing -> Left $ "Couldn't find variable " ++ quote name

reduce :: Expr -> Either Error Expr
reduce = reduceEnv M.empty

reduceEnv :: Env -> Expr -> Either Error Expr
reduceEnv env (Var var) = envLookup env var
reduceEnv env (Lam arg body) = Right $ Cls arg body env
reduceEnv env (App func expr) = do
  app <- reduceEnv env func
  case app of
    Cls arg body closedEnv -> do
      evaluated <- reduceEnv env expr
      let env' = M.insert arg evaluated $ closedEnv `M.union` env
      reduceEnv env' body

    other -> Left $
      "Can't apply " ++ quote (prettify other)
      ++ " to " ++ quote (prettify expr)
reduceEnv _ cls = Right cls

eval :: String -> IO ()
eval expr =
  case readExpr expr of
    Left err -> putStr $ errorBundlePretty err
    Right res ->
      case reduce res of
        Left err -> putStrLn err
        Right red -> putStrLn (prettify red)
