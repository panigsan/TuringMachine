module Util where

import TuringMachine
import System.IO

importTM :: [String] -> Machine
importTM x = 
    Machine { partFun      = map (parseFun) functions
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
parseFun :: [String] -> PartFun
parseFun values = 
    PartFun 
          { input  = (values !! 0 :: State, values !! 1 !! 0 :: Symbol)
          , action = (values !! 3 :: State, values !! 4 !! 0 :: Symbol, 
                      read (values !! 5) :: Direction)
          }
