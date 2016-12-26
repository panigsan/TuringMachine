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

highLight :: Bool -> IO ()
highLight x = setSGR[SetSwapForegroundBackground x]


highLightFun :: Machine -> PartFun -> Bool -> IO ()
highLightFun tm fun bool= do
    let (Just index) = fun `elemIndex` (partFun tm)

    setCursorPosition (10 + index) 0
    highLight bool
    renderFun fun
    highLight False


renderTapeContainer :: IO ()
renderTapeContainer = do
    setCursorPosition 0 0

    putStrLn $ "╔" ++ concat (replicate 18 "══")
                   ++ "═╦═╦"
                   ++ concat (replicate 18 "══")
                   ++ "═╗"

    putStrLn   $ "║" ++ replicate 77 ' ' ++ "║"

    putStrLn $ "╚" ++ concat (replicate 18 "══")
                   ++ "═╩═╩"
                   ++ concat (replicate 18 "══")
                   ++ "═╝"

renderTapeContent :: Tape -> IO ()
renderTapeContent t = do
   setCursorPosition 1 1

   putStr . repl . intersperse ' ' . reverse . trail $ left t

   putStr "║"
   setSGR [SetColor Foreground Vivid Green]
   putChar . repl' $ cursor t
   setSGR [Reset]
   putStr "║"

   putStr . repl . intersperse ' ' . trail $ right t

   where
       -- equal spaces on each side
       sides = 19
       -- add or remove symbols in order to keep both sides of the same length
       trail text | (length text < sides) = text ++ replicate (sides - length text) blank
                  | otherwise = take sides text
       repl = map (repl')
       repl' '_' = ' '
       repl'  c  = c

renderTMContainer :: Machine -> IO ()
renderTMContainer tm = do
    setCursorPosition 5 0

    putStrLn $ "╔════════════════════╦══════════════════════════════════╗"
    putStrLn $ "║       Input        ║              Output              ║"
    putStrLn $ "╟────────────────────╫──────────────────────────────────╢"
    putStrLn $ "║  State  │  Symbol  ║  State  │  Symbol  │  Direction  ║"
    putStrLn $ "╠════════════════════╬══════════════════════════════════╣"
    --              9          10         9         10           13

    mapM_ renderFun $ partFun tm
    putStrLn $ "╚════════════════════╩══════════════════════════════════╝"

renderFun :: PartFun -> IO ()
renderFun fun = do
    let (state, symbol)               = input  fun
        (state', symbol', direction') = action fun

    putStr $ "║"

    putStr $ fill state 9                ++ "│"
    putStr $ fill [symbol] 10            ++ "║"

    putStr $ fill state' 9               ++ "│"
    putStr $ fill [symbol'] 10           ++ "│"
    putStr $ fill (show direction') 13   ++ "║"
    putStrLn $ ""


    where
          fill :: String -> Int -> String
          fill s x = " " ++ s ++ replicate (x - length s - 1) ' '
