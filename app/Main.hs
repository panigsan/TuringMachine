module Main where

import Lib
import TuringMachine
import System.IO    (BufferMode (NoBuffering), hSetBuffering, stdout)


execute :: State -> IO()
execute state = do
  putStr "Symbol on tape: "
  symbol <- getChar :: IO Char
  putStrLn ""

  let (state', symbol', direction') = (partFun tm) state symbol
  putStrLn $ show $ (state', symbol', direction')
  if not $ state' `elem` (finalStates tm)
      then execute state'
      else putStrLn "ended"

  

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let state = initialState tm
  execute state

