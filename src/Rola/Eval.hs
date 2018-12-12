module Rola.Eval
  ( eval
  , reduce
  ) where

import qualified Data.Map as M
import Rola.Parser
import Rola.Pretty
import Rola.Syntax
import Text.Megaparsec (errorBundlePretty)

envLookup :: Env -> Name -> Expr
envLookup env name =
  case M.lookup name env of
    Just expr -> expr
    Nothing -> error $ "Couldn't find variable " ++ name

reduce :: Expr -> Expr
reduce = reduceEnv M.empty

reduceEnv :: Env -> Expr -> Expr
reduceEnv env (Var var) = envLookup env var
reduceEnv env (Lam arg body) = Cls arg body env
reduceEnv env (App func expr) =
  case reduceEnv env func of
    Cls arg body closedEnv ->
      let evaluated = reduceEnv env expr
          env' = M.insert arg evaluated $ closedEnv `M.union` env
      in reduceEnv env' body
    _ -> error "Can't apply this shit"
reduceEnv _ cls = cls

eval :: String -> IO ()
eval expr =
  case readExpr expr of
    Right res -> putStrLn $
      let reduced = reduce res
      in prettify reduced ++ "\n -> " ++ show reduced
    Left err -> putStr $ errorBundlePretty err
