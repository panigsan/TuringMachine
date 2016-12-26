module Main where

import System.Console.ANSI
import Control.Concurrent
import TuringMachine
import Util
import Render
import Data.List
import System.IO    --(BufferMode (NoBuffering), hSetBuffering, stdout, hFlush)

main :: IO ()
main = do
    let fileName = "adder_adv.txt"
    file <- readFile fileName

    let tm = importTM (lines file)
        state = initialState tm
        tape = initTape "11101"
        results = compute tm state tape

    resetScreen
    hideCursor

    renderTapeContainer
    renderTapeContent tape
    renderTMContainer tm

    mapM_ (\fun -> updateScreen tm fun) results

    showCursor
    renderTMContainer tm

updateScreen :: Machine -> (Maybe PartFun, Tape) -> IO ()
updateScreen tm (Nothing, tape) = do
    renderTapeContent tape
    pause
updateScreen tm (Just fun, tape) = do
    renderTapeContent tape

    highLightFun tm fun True
    pause
    highLightFun tm fun False
