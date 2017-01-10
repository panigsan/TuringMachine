-- | Contains useful functions to render the Turing Machine
module Render where

import TuringMachine
import System.IO
import System.Console.ANSI
import Control.Concurrent
import Data.List

-- | Clear the screen and move the cursor to 0 0
resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

-- | Pause the execution for 1 second
pause :: Int -> IO ()
pause millis = do
    hFlush stdout
    -- 1 second pause
    threadDelay $ millis * 1000

-- | Turn the highlight ON (swap the foreground color with the background)
highLightON :: IO ()
highLightON = setSGR[SetSwapForegroundBackground True]
-- | Turn the highlight OFF
highLightOFF :: IO ()
highLightOFF = setSGR[SetSwapForegroundBackground False]

-- | Turn ON the bold
boldON :: IO ()
boldON = setSGR [SetConsoleIntensity BoldIntensity]
-- | Turn OFF the bold
boldOFF :: IO ()
boldOFF = setSGR [SetConsoleIntensity NormalIntensity]

-- | Print the header on the screen
renderHeader :: String -- ^ File name
             -> Tape   -- ^ Initial tape
             -> IO ()
renderHeader fileName tape = do
    boldON >> (putStr $ "Turing machine: ") >> boldOFF
    putStrLn fileName
    setCursorPosition 1 0 >> renderTapeContainer
    setCursorPosition 2 1 >> renderTapeContent tape

-- | Print the borders of the tape container
renderTapeContainer :: IO ()
renderTapeContainer = do
    putStrLn $ "╔" ++ concat (replicate 18 "══")
                   ++ "═╦═╦"
                   ++ concat (replicate 18 "══")
                   ++ "═╗"

    putStrLn   $ "║" ++ replicate 77 ' ' ++ "║"

    putStrLn $ "╚" ++ concat (replicate 18 "══")
                   ++ "═╩═╩"
                   ++ concat (replicate 18 "══")
                   ++ "═╝"

-- | Print the tape. Make sure to move the cursor inside the container
renderTapeContent :: Tape -- ^ Tape to be printed
                  -> IO ()
renderTapeContent tape = do
   putStr . map repl . intersperse ' ' . reverse . trail . left $ tape

   putStr "║"
   boldON
   putChar . repl . cursor $ tape
   setSGR [Reset]
   putStr "║"

   putStr . map repl . intersperse ' ' . trail . right $ tape

   where
       -- equal spaces on each side
       sides = 19
       -- add or remove symbols in order to keep both sides of the same length
       trail text | (length text < sides) = text ++ replicate (sides - length text) blank
                  | otherwise = take sides text
       repl '_' = ' '
       repl  c  = c

-- | Print the given turing machine and all its partial functions
renderTMContainer :: Machine -- ^ Turing machine to be printed
                  -> IO ()
renderTMContainer tm = do
    putStrLn $ "╔════════════════════╦══════════════════════════════════╗"
    putStrLn $ "║       Input        ║              Output              ║"
    putStrLn $ "╟────────────────────╫──────────────────────────────────╢"
    putStrLn $ "║  State  │  Symbol  ║  State  │  Symbol  │  Direction  ║"
    putStrLn $ "╠════════════════════╬══════════════════════════════════╣"
    --              9          10         9         10           13

    mapM_ renderFun $ partFun tm
    putStrLn $ "╚════════════════════╩══════════════════════════════════╝"

-- | Print a partial function matching the tm container sizes
renderFun :: PartFun -- ^ Partial function to be printed
          -> IO ()
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
