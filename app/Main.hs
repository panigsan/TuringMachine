module Main where

import System.Console.ANSI
import Control.Concurrent
import TuringMachine
import Util (importTM)
import Render
import Data.List (intercalate, elemIndex)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout, hFlush)

main :: IO ()
main = do
    resetScreen
    putStrLn $ "File name:"
    fileName <- getLine

    putStrLn $ "Tape: "
    tapeText <- getLine

    file <- readFile fileName
    let
        mtm = importTM file
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

        resetScreen
        hideCursor

        renderHeader fileName tape

        setCursorPosition 4 0 >> renderTapeContainer

        setCursorPosition 7 0 >> renderTMContainer tm

        setCursorPosition 5 1 >> renderTapeContent tape

        mapM_ (\(step, fun) -> updateStepCounter step >>
                               updateScreen tm fun) (zip [1..] results)

        setCursorPosition 7 0 >> renderTMContainer tm
        showCursor

-- | Prints the tape content and (if possible) highlights the partial function
updateScreen :: Machine               -- ^ Turing machine
             -> (Maybe PartFun, Tape) -- ^ Curerent partial function used
             -> IO ()
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

-- | Update the step counter
updateStepCounter :: Int -- ^ Number of step
                  -> IO ()
updateStepCounter step = do
    setCursorPosition 8 60
    boldON >> putStr "Step: " >> boldOFF
    putStr $ show step
