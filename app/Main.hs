module Main where

import Control.Monad
import Rola
import System.IO

main :: IO ()
main = repl

repl :: IO ()
repl = do
  input <- putStr "ROLA> " >> hFlush stdout >> getLine
  unless (input == ":q" || input == ":quit") $ do
    eval input
    repl
