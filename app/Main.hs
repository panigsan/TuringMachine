module Main where

import Lib
import TuringMachine
import Util
import System.IO    (BufferMode (NoBuffering), hSetBuffering, stdout)

-- Simple TM
fun :: [PartFun]
fun = initPartFun 
            [ ( ("q0", '0'), ("q0", '0', R) )
            , ( ("q0", '1'), ("q0", '1', R) )
            , ( ("q0", ' '), ("q1", ' ', L) )
            , ( ("q1", '0'), ("q2", '1', S) )
            , ( ("q1", '1'), ("q1", '0', L) )
            ]

tm1  = Machine
     { states       = ["q0", "q1", "q2"]
     , tapeAlphabet = ['0', '1', ' ']
     , blankSymbol  = ' '
     , inputSymbol  = ['0', '1']
     , partFun      = fun
     , initialState = "q0"
     , finalStates  = ["q2"]
     }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "File: "
    fa <- readFile <- getLine
    fileName <- getLine
    putStrLn "Tape: "
    tapeStr  <- getLine

    file     <- readFile fileName
    let tm = importTM (lines file)
        state = initialState tm
        tape  = initTape tapeStr (blankSymbol tm)
    putStrLn "Start"
    putStrLn $ "  " ++ fancyTape tape 21
    putStrLn "===="
    execute tm1 tape state

