module Render where

import TuringMachine
import System.IO
import System.Console.ANSI
import Control.Concurrent
import Data.List

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

pause :: IO ()
pause = do
    hFlush stdout
    -- 1 second pause
    threadDelay 1000000

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

-- Conver the tape to a single string with Int visible symbols
renderTapeContent :: Tape -> Int -> IO ()
renderTapeContent t x = do
    putStr . repl . intersperse ' ' . reverse . trail $ left t
    putStr "║"
    setSGR [SetColor Foreground Dull Green]
    putChar . repl' $ cursor t
    setSGR [SetColor Foreground Dull White]
    putStr "║"
    putStr . repl . intersperse ' ' . trail $ right t
    where
        -- equal spaces on each side
        sides = (x - 1) `div` 2
        -- add or remove symbols in order to keep both sides of the same length
        trail text | (length text < sides) = text ++ replicate (sides - length text) blank
                   | otherwise = take sides text
        repl = map (repl')
        repl' '_' = ' '
        repl'  c  = c

renderTape :: Tape -> IO ()
renderTape tape = do
    setCursorPosition 0 0

    putStrLn $ "╔" ++ concat (replicate 18 "══")
                   ++ "═╦═╦"
                   ++ concat (replicate 18 "══")
                   ++ "═╗"

    putStr   $ "║"
    --putStr   $ fancyTape tape 39
    renderTapeContent tape 39
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
    putStrLn $ "╠════════════════════╬══════════════════════════════════╣"
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
    let (state, symbol)               = input  fun
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
