module Util where

import TuringMachine
import System.IO
import Data.Maybe
import Text.Read

importTM :: [String] -> Machine
importTM x =
    Machine { partFun      = keepJust . map (parseFun) $ functions
            , initialState = words initialLine !! 2
            , finalStates  = parseFinalStates finalLine
            }
    where
        initialLine = x !! 0
        finalLine   = x !! 1
        functions   = filter (\x -> length x == 6 && x !! 0 /= "#") (map words (drop 2 x))


-- "Final = 2 3" -> ["2", "3"]
parseFinalStates :: String -> [State]
parseFinalStates x = drop 2 . words $ x

-- ["0", "_", "=>", "1", "_", "L"] ->
parseFun :: [String] -> Maybe PartFun
parseFun values = do
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
        readMaybeSymbol _ = Nothing

keepJust :: [Maybe PartFun] -> [PartFun]
keepJust [Nothing] = []
keepJust [Just x]  = [x]
keepJust (x:xs) = keepJust [x] ++ keepJust xs
