module Main (main) where

import System.Console.ANSI
import TuringMachine
import Util       (importTM)
import Render
import Text.Read  (readMaybe)
import Data.Maybe (fromJust)
import Data.List  (intercalate, elemIndex)
import System.IO  (BufferMode (NoBuffering), hSetBuffering, stdout, hFlush)

main :: IO ()
main = do
    resetScreen
    putStrLn $ "File name:"
    fileName <- getLine

    putStrLn $ "Tape: "
    tapeText <- getLine

    putStrLn $ "Interval (in millisecond): "
    millisText <- getLine

    file <- readFile fileName
    let
        mtm = importTM file
    if mtm == Nothing then do
        putStrLn $ "Cannot parse the turing machine"
    else do
        let millis = parseMillis millisText
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

        mapM_ (\(step, fun) -> updateScreen tm fun step millis)
              $ zip [1..] results

        setCursorPosition 7 0 >> renderTMContainer tm
        showCursor

    putStrLn $ "Press enter to continue (q to quit)"
    a <- getLine
    if a /= "q" then main
    else return ()
    
    where
        parseMillis :: String -> Int
        parseMillis x | millis == Nothing = 1000
                      | otherwise         = fromJust millis
            where millis = readMaybe $ x :: Maybe Int

-- | Prints the tape content and (if possible) highlights the partial function
updateScreen :: Machine               -- ^ Turing machine
             -> (Maybe PartFun, Tape) -- ^ Curerent partial function used
             -> Int                   -- ^ Step counter
             -> Int                   -- ^ Interval (time to wait)
             -> IO ()
updateScreen tm (Nothing, tape) step interval = do
    setCursorPosition 5 1 >> renderTapeContent tape
    updateStepCounter step
    pause interval

updateScreen tm (Just fun, tape) step interval = do
    setCursorPosition 5 1 >> renderTapeContent tape
    updateStepCounter step

    let (Just index) = fun `elemIndex` (partFun tm)

    setCursorPosition (12 + index) 0
    highLightON >> (renderFun fun)

    pause interval

    setCursorPosition (12 + index) 0
    highLightOFF >> (renderFun fun)

-- | Update the step counter
updateStepCounter :: Int -- ^ Number of step
                  -> IO ()
updateStepCounter step = do
    setCursorPosition 8 60
    boldON >> putStr "Step: " >> boldOFF
    putStr $ show step
