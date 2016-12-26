module Main where

import System.Console.ANSI
import Control.Concurrent
import TuringMachine
import Util
import Render
import Data.List
import System.IO--    (BufferMode (NoBuffering), hSetBuffering, stdout, hFlush)


main :: IO ()
main = do
    let fileName = "adder_adv.txt"
    file <- readFile fileName

    let tm = importTM (lines file)
        state = initialState tm
        tape = initTape "101001110"
        results = compute tm state tape

    resetScreen
    renderTape tape
    pause
    render tm results
