module Main where

import Lib
import TuringMachine
import System.IO    (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Hello: "
  number <- readLn :: IO Double
  main
