module Rola.Eval
  ( eval
  ) where

import Rola.Parser
import Rola.Pretty
import Rola.Syntax
import Text.Megaparsec (errorBundlePretty)

reduce :: Expr -> Expr
reduce = id

eval :: String -> IO ()
eval expr =
  case readExpr expr of
    Right res -> putStrLn $ prettify res ++ "\n  -> " ++ show res
    Left err -> putStr $ errorBundlePretty err
