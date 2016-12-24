module Main where

import System.Console.ANSI
import Control.Concurrent
import TuringMachine
import Util
import Data.List
import System.IO--    (BufferMode (NoBuffering), hSetBuffering, stdout, hFlush)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "File: "
    fileName <- getLine
    file     <- readFile fileName

    putStrLn "Tape: "
    tapeStr  <- getLine

    let tm = importTM (lines file)
        state = initialState tm
        tape  = initTape tapeStr 

    putStrLn $ fancyTape tape 21
    putStrLn "===="
    --execute tm tape state
    let result = compute tm state tape 
    putStrLn $ concat $ map (\(_, tape) -> fancyTape tape 21 ++ "\n") result

pause :: IO ()
pause = do
    hFlush stdout
    -- 1 second pause
    threadDelay 1000000

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

main2 :: IO ()
main2 = do
    hSetBuffering stdout NoBuffering
    putStrLn "File: "
    fileName <- getLine
    file     <- readFile fileName

    putStrLn "Tape: "
    tapeStr  <- getLine

    let tm      = importTM (lines file)
        state   = initialState tm
        tape    = initTape tapeStr
        results = compute tm state tape
    
    resetScreen
    render tm results
    
main3 :: IO ()
main3 = do
    let fileName = "adder.txt"
    file <- readFile fileName
    
    let tm = importTM (lines file)
        state = initialState tm
        tape = initTape "0"
        results = compute tm state tape

    resetScreen
    renderTape tape
    pause
    render tm results

renderTape :: Tape -> IO ()
renderTape tape = do
    setCursorPosition 0 0

    putStrLn $ "╔" ++ concat (replicate 18 "═╤") 
                   ++ "═╦═╦"
                   ++ concat (replicate 18 "═╤") 
                   ++ "═╗"

    putStr   $ "║"
    putStr   $ fancyTape tape 39
    putStrLn $ "║"

    putStrLn $ "╚" ++ concat (replicate 18 "═╧") 
                   ++ "═╩═╩"
                   ++ concat (replicate 18 "═╧") 
                   ++ "═╝"

renderTM :: Machine -> PartFun -> IO ()
renderTM tm fun = do
    setCursorPosition 5 0
    putStrLn "000"

render :: Machine -> [(PartFun, Tape)] -> IO ()
render tm [] = return ()
render tm ((fun, tape):xs) = do
    --resetScreen

    renderTape tape
    renderTM tm fun

    pause
    render tm xs

