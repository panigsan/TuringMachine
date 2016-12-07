{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Util where

import TuringMachine
import System.IO

--importTM :: String -> Machine

importTM x = 
    Machine { states       = []
            , tapeAlphabet = []
            , blankSymbol  = '_' --blankLine !! 2 !! 0
            , inputSymbol  = []
            , partFun      = map (parseFun) functions
            , initialState = "0"
            , finalStates  = ["2"]
            }
    where
        blankLine = x !! 0
        initLine  = x !! 1
        finalLine = x !! 2
        functions = drop 3 x

parseFun :: String -> PartFun
parseFun x = 
    PartFun 
          { input  = (values !! 0 :: State, values !! 1 !! 0 :: Symbol)
          , output = (values !! 3 :: State, values !! 4 !! 0 :: Symbol, 
                      read (values !! 5) :: Direction)
          }
    where values = words x
