module Main where

import Lib
import TuringMachine
import System.IO    (BufferMode (NoBuffering), hSetBuffering, stdout)

-- Simple TM
fun :: PartFun

fun 0 '0' = (0, '0', R)
fun 0 '1' = (0, '1', R)
fun 0 ' ' = (1, ' ', L)

fun 1 '0' = (2, '1', S)
fun 1 '1' = (1, '0', L)

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
        tape  = initTape "1011" (blankSymbol tm1)
    putStrLn "Start"
    putStrLn $ fancyTape tape 10
    putStrLn "===="
    execute tm1 tape state

