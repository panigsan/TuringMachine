module Util where

import TuringMachine
import System.IO

--importTM :: String -> Machine

importTM x = 
    Machine { partFun      = map (parseFun) functions
            , initialState = "0"
            , finalStates  = ["2"]
            }
    where
        finalLine = x !! 1
        functions = drop 2 x

parseFun :: String -> PartFun
parseFun x = 
    PartFun 
          { input  = (values !! 0 :: State, values !! 1 !! 0 :: Symbol)
          , output = (values !! 3 :: State, values !! 4 !! 0 :: Symbol, 
                      read (values !! 5) :: Direction)
          }
    where values = words x
