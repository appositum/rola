module Rola.Pretty where

class Pretty p where
  {-# MINIMAL prettify #-}
  prettify :: p -> String

  prettyPrint :: p -> IO ()
  prettyPrint = putStrLn . prettify
