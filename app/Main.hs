module Main where

import Lib
import TuringMachine
import System.IO    (BufferMode (NoBuffering), hSetBuffering, stdout)


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let state = initialState tm1
        tape  = initTape "1011" (blankSymbol tm1)
    putStrLn "Start"
    putStrLn $ fancyTape tape 10
    putStrLn "===="
    execute tm1 tape state

