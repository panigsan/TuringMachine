-- | Contains useful functions for importing a turing machine from a file
module Util where

import TuringMachine
import System.IO
import Data.Maybe
import Text.Read

-- | Parse a turing machine. Example of a format:
--
-- > Initial = 0
-- > Finals = 2 3
-- > # this is a comment
-- > 0 _ => 1 _ L
-- > 0 * => 0 * R
-- >
-- > # this is also a comment
-- > 1 0 => 2 1 S
-- > 1 1 => 1 0 L
-- It is important that the first two lines look like the example above
importTM :: String        -- ^ Text following the syntax above
         -> Maybe Machine -- ^ Resulting turing machine
importTM x = do
    let l = lines x
        initialLine = words $ l !! 0
        finalLine   = words $ l !! 1
        functions   = keepJust . map parseFun $ drop 2 l

    if length initialLine /= 3 ||
       length finalLine < 3
    then Nothing
    else
        Just Machine { partFun      = functions
                     , initialState = initialLine !! 2
                     , finalStates  = drop 2 finalLine -- remove "Final ="
                     }

-- | Parse a function. It has to follow this syntax
--
-- > input_state input_symbol => output_state output_character direction
-- Example:
--
-- > 0 _ => 1 _ L
parseFun :: String        -- ^ Line to parseFun
         -> Maybe PartFun -- ^ Resulting partial function
parseFun line
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
            values = words line
            readMaybeSymbol :: String -> Maybe Symbol
            readMaybeSymbol [x] = Just x
            readMaybeSymbol _   = Nothing

-- | Extract all the partial functions, leaving out the ones which couldn't
-- been parsed correctly
keepJust :: [Maybe PartFun] -> [PartFun]
keepJust [] = []
keepJust [Nothing] = []
keepJust [Just x]  = [x]
keepJust (x:xs) = keepJust [x] ++ keepJust xs
