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
        mtm = importTM (lines file)
    if mtm == Nothing then do
        putStrLn $ "Cannot parse the turing machine"
        putStrLn $ "Press enter to continue"
        a <- getLine
        main
    else do
        let
            (Just tm) = mtm
            state = initialState tm
            tape = initTape tapeText
            results = compute tm state tape

        mapM_ (\r -> putStrLn $ show r) results
        --putStrLn $ show results
        a <- getLine

        resetScreen
        hideCursor

        renderHeader fileName tape

        setCursorPosition 4 0 >> renderTapeContainer

        setCursorPosition 7 0 >> renderTMContainer tm

        setCursorPosition 5 1 >> renderTapeContent tape

        mapM_ (\fun -> updateScreen tm fun) results

        showCursor
        setCursorPosition 7 0 >> renderTMContainer tm

updateScreen :: Machine -> (Maybe PartFun, Tape) -> IO ()
updateScreen tm (Nothing, tape) = do
    setCursorPosition 5 1 >> renderTapeContent tape
    pause
updateScreen tm (Just fun, tape) = do
    setCursorPosition 5 1 >> renderTapeContent tape

    let (Just index) = fun `elemIndex` (partFun tm)

    setCursorPosition (12 + index) 0
    highLightON >> (renderFun fun)
    pause

    setCursorPosition (12 + index) 0
    highLightOFF >> (renderFun fun)
