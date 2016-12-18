module TuringMachine where

type State      = String
type Symbol     = Char
data Direction  = L | R | S -- Left | Right | Stay
    deriving (Show, Eq, Read)

data PartFun    = PartFun
                  { input       :: (State, Symbol)
                  , output      :: (State, Symbol, Direction)
                  }
    deriving (Show)

data Machine    = Machine 
                  { partFun        :: [ PartFun ]
                  , initialState   :: State
                  , finalStates    :: [ State]
                  }
    deriving (Show)

data Tape = Tape
            { left      :: [ Symbol ]
            , cursor    :: Symbol
            , right     :: [ Symbol ]
            }
    deriving (Show, Eq)

blank = '_' :: Symbol

-- Initialize a new tape with the given string
initTape :: [ Symbol ] -> Tape
initTape []       = Tape
                    { left    = []
                    , cursor  = blank
                    , right   = []
                    }
initTape (x:xs)   = Tape
                    { left    = []
                    , cursor  = x
                    , right   = xs
                    }

-- Move cursor on the tape
moveCursor :: Tape -> Direction -> Tape

moveCursor t R = t { left   = cursor t : (left t)
                   , cursor = if null $ right t
                                  then blank
                                  else head $ right t
                   , right  = if null $ right t
                                  then ""
                                  else tail $ right t
                   }

moveCursor t L = t { left    = if null $ left t
                                   then ""
                                   else tail $ left t
                    , cursor = if null $ left t
                                   then blank
                                   else head $ left t
                    , right   = cursor t : (right t)
                    }

moveCursor t S = t

-- Conver the tape to a single string with Int visible symbols
fancyTape :: Tape -> Int -> String
fancyTape t x = map (repl) $
                    (reverse $ trail $ left t)     ++
                    [ '|', cursor t, '|' ]         ++
                    trail (right t)
            where
                -- equal spaces on each side
                sides = (x - 1) `div` 2
                -- add or remove symbols in order to keep both sides of the same length
                trail text | (length text < sides) = text ++ replicate (sides - length text) blank
                           | otherwise = take sides text
                repl '_' = ' '
                repl  c  = c
                

finished :: Machine -> State -> Bool
finished tm state = state `elem` (finalStates tm)

next :: Machine -> (State, Symbol) -> (State, Symbol, Direction)
next machine (x_state, x_symbol) = repl . output . head . filter 
                    (\f -> input f `compare` (x_state, x_symbol)) $ partFun machine

        where 
            compare (state, '*') (state', _) = state == state'
            compare a b                      = a == b 

            repl (state, '*', dir)    = (state, x_symbol, dir)
            repl (state, symbol, dir) = (state, symbol, dir)

execute :: Machine -> Tape -> State -> IO()
execute tm tape state = do
    putStrLn $ show state ++ ":" ++ fancyTape tape 21
    if tm `finished` state
    then
        putStrLn "finished"
    else
        let symbol = cursor tape
            (state', symbol', direction') = tm `next` (state, symbol)
            tape' = tape { cursor = symbol' } `moveCursor` direction'
        in execute tm tape' state'
