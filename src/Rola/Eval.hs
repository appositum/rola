module Rola.Eval
  ( churchEncodings
  , envLookup
  , eval
  , reduce
  , reduceInEnv
  ) where

import Church (encodings)
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

reduceInEnv :: Env -> Expr -> Either Error Expr
reduceInEnv env (Var var) = envLookup env var
reduceInEnv env (Lam arg body) = Right $ Cls arg body env
reduceInEnv env (App func expr) = do
  app <- reduceInEnv env func

  case app of
    Cls arg body closedEnv -> do
      evaluated <- reduceInEnv env expr
      let env' = M.insert arg evaluated $ closedEnv `M.union` env
      reduceInEnv env' body

    other -> Left $
      "Can't apply " ++ quote (prettify other)
      ++ " to " ++ quote (prettify expr)

reduceInEnv _ other = Right other

churchEncodings :: Env
churchEncodings =
  let fromRight (Right a) = a
      mapper = fromRight . reduceInEnv M.empty . fromRight . readExpr
  in mapper <$> M.fromList encodings

reduce :: Expr -> Either Error Expr
reduce = reduceInEnv churchEncodings

eval :: String -> IO ()
eval expr =
  case readExpr expr of
    Left err -> putStr $ errorBundlePretty err
    Right res ->
      case reduce res of
        Left err -> putStrLn err
        Right red -> putStrLn (prettify red)
