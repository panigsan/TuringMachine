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
    resetScreen
    putStrLn $ "File name:"
    fileName <- getLine

    putStrLn $ "Tape: "
    tapeText <- getLine

    file <- readFile fileName
    let
        tm = importTM (lines file)
        state = initialState tm
        tape = initTape tapeText
        results = compute tm state tape

    resetScreen
    hideCursor

    renderHeader fileName tape

    setCursorPosition 6 0 >> (putStrLn $ replicate 78 '-')

    boldON >> (putStrLn "Output") >> boldOFF

    setCursorPosition 8 0 >> renderTapeContainer

    setCursorPosition 12 0 >> renderTMContainer tm

    setCursorPosition 9 1 >> renderTapeContent tape

    mapM_ (\fun -> updateScreen tm fun) results

    showCursor
    setCursorPosition 12 0 >> renderTMContainer tm

updateScreen :: Machine -> (Maybe PartFun, Tape) -> IO ()
updateScreen tm (Nothing, tape) = do
    setCursorPosition 9 1 >> renderTapeContent tape
    pause
updateScreen tm (Just fun, tape) = do
    setCursorPosition 9 1 >> renderTapeContent tape

    let (Just index) = fun `elemIndex` (partFun tm)

    setCursorPosition (17 + index) 0
    highLightON >> (renderFun fun)
    pause
    
    setCursorPosition (17 + index) 0
    highLightOFF >> (renderFun fun)
