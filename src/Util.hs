module Util where

import TuringMachine
import System.IO
import Data.Maybe
import Text.Read

importTM :: [String] -> Maybe Machine
importTM x = do
    let initialLine = words $ x !! 0
        finalLine   = words $ x !! 1
        functions   = map (parseFun . words) (drop 2 x)

    if length initialLine /= 3 ||
       length finalLine < 3
    then Nothing
    else
        Just Machine { partFun      = keepJust $ functions
                     , initialState = initialLine !! 2
                     , finalStates  = parseFinalStates finalLine
                     }


-- ["Final", "=", "2", "3"] -> ["2", "3"]
parseFinalStates :: [String] -> [State]
parseFinalStates x = drop 2 x

-- ["0", "_", "=>", "1", "_", "L"] -> Maybe PartFun
parseFun :: [String] -> Maybe PartFun
parseFun values
    | length values /= 6 || values !! 0 !! 0 == '#' = Nothing
    | otherwise = do
        let input_state   = values !! 0
            input_symbol  = readMaybeSymbol $ values !! 1

            action_state  = values !! 3
            action_symbol = readMaybeSymbol $ values !! 4
            action_dir    = readMaybe $ values !! 5 :: Maybe Direction

        if input_symbol  == Nothing ||
           action_symbol == Nothing ||
           action_dir    == Nothing

           then Nothing
           else Just (PartFun (input_state, fromJust input_symbol)
                              (action_state, fromJust action_symbol, fromJust action_dir))
        where
            readMaybeSymbol :: String -> Maybe Symbol
            readMaybeSymbol [x] = Just x
            readMaybeSymbol _   = Nothing

keepJust :: [Maybe PartFun] -> [PartFun]
keepJust [Nothing] = []
keepJust [Just x]  = [x]
keepJust (x:xs) = keepJust [x] ++ keepJust xs
