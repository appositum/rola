module Rola.Pretty where

class Pretty p where
  {-# minimal prettify #-}
  prettify :: p -> String

  prettyPrint :: p -> IO ()
  prettyPrint = putStrLn . prettify
