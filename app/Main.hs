module Main where

import Lib
import TuringMachine
import System.IO    (BufferMode (NoBuffering), hSetBuffering, stdout)


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let state = initialState tm1
    execute tm1 state

