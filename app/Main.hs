module Main where

import TuringMachine
import Util
import Data.List
import System.IO    (BufferMode (NoBuffering), hSetBuffering, stdout)

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

