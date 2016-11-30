module Main where

import Lib
import TuringMachine
import Util
import System.IO    (BufferMode (NoBuffering), hSetBuffering, stdout)

-- Simple TM
fun :: [PartFun]
fun = initPartFun 
            [ ( (0, '0'), (0, '0', R) )
            , ( (0, '1'), (0, '1', R) )
            , ( (0, ' '), (1, ' ', L) )
            , ( (1, '0'), (2, '1', S) )
            , ( (1, '1'), (1, '0', L) )
            ]

tm1  = Machine
     { states       = [0, 1, 2]
     , tapeAlphabet = ['0', '1', ' ']
     , blankSymbol  = ' '
     , inputSymbol  = ['0', '1']
     , partFun      = fun
     , initialState = 0
     , finalStates  = [2]
     }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let state = initialState tm1
        tape  = initTape "101011" (blankSymbol tm1)
    putStrLn "Start"
    putStrLn $ "  " ++ fancyTape tape 21
    putStrLn "===="
    execute tm1 tape state

