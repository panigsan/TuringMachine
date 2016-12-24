module Main where

import System.Console.ANSI
import Control.Concurrent
import TuringMachine
import Util
import Data.List
import System.IO--    (BufferMode (NoBuffering), hSetBuffering, stdout, hFlush)
main3 :: IO ()
main3 = do
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
{-
    putStrLn "===="
    --execute tm tape state
    let result = compute tm state tape 
    putStrLn $ concat $ map (\(_, tape) -> fancyTape tape 21 ++ "\n") result
-}
pause :: IO ()
pause = do
    hFlush stdout
    -- 1 second pause
    threadDelay 1000000

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0
{-
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
    render tm results-}
    
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

renderTape :: Tape -> IO ()
renderTape tape = do
    setCursorPosition 0 0

    putStrLn $ "╔" ++ concat (replicate 18 "══") 
                   ++ "═╦═╦"
                   ++ concat (replicate 18 "══") 
                   ++ "═╗"

    putStr   $ "║"
    putStr   $ fancyTape tape 39
    putStrLn $ "║"

    putStrLn $ "╚" ++ concat (replicate 18 "══") 
                   ++ "═╩═╩"
                   ++ concat (replicate 18 "══") 
                   ++ "═╝"

renderTM :: Machine -> PartFun -> IO ()
renderTM tm fun = do
    setCursorPosition 5 0

    putStrLn $ "╔════════════════════╦══════════════════════════════════╗"
    putStrLn $ "║       Input        ║              Output              ║"
    putStrLn $ "╟────────────────────╫──────────────────────────────────╢"
    putStrLn $ "║  State  │  Symbol  ║  State  │  Symbol  │  Direction  ║"
    putStrLn $ "╟════════════════════╫══════════════════════════════════╢"
    --              9          10         9         10           13

    sequence . map (\f -> setHighlight (f==fun) >> renderFun f ) $ partFun tm
    setHighlight False
    putStrLn $ "╚════════════════════╩══════════════════════════════════╝"

setHighlight :: Bool -> IO ()
setHighlight True = do
    setSGR [SetColor Background Dull White]
    setSGR [SetColor Foreground Dull Black]
setHighlight False = do
    setSGR [SetColor Background Dull Black]
    setSGR [SetColor Foreground Dull White]

renderFun :: PartFun -> IO ()
renderFun fun = do
    let (state, symbol) = input fun
        (state', symbol', direction') = action fun
    putStr $ "║"

    putStr $ fill state 9                ++ "│"
    putStr $ fill [symbol] 10            ++ "║"
 
    putStr $ fill state' 9               ++ "│"
    putStr $ fill [symbol'] 10           ++ "│"
    putStr $ fill (show direction') 13   ++ "│"
    putStrLn $ ""

    where
        fill :: String -> Int -> String
        fill s x = " " ++ s ++ replicate (x - length s - 1) ' '

render :: Machine -> [(Maybe PartFun, Tape)] -> IO ()
render tm [] = return ()
render tm ((Nothing, tape):xs) = do
    renderTape tape
    render tm xs
render tm ((Just fun, tape):xs) = do
    renderTape tape
    renderTM tm fun

    pause
    --a <- getLine
    render tm xs

