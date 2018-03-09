module Main where

import Fish
import Image
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file, n] -> writeImage file $ squarelimit $ read n
    _         -> writeImage "test.png" $ squarelimit 3
