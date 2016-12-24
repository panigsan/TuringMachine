
module TuringMachine where
import Data.List

type State      = String
type Symbol     = Char
type Input      = (State, Symbol)
type Action     = (State, Symbol, Direction)
data Direction  = L | R | S -- Left | Right | Stay
    deriving (Show, Eq, Read)

data PartFun    = PartFun
                  { input       :: Input
                  , action      :: Action
                  }
    deriving (Show, Eq)

data Machine    = Machine 
                  { partFun        :: [ PartFun ]
                  , initialState   :: State
                  , finalStates    :: [ State]
                  }
    deriving (Show, Eq)

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
                    intersperse '│' (reverse $ trail $ left t)
                 ++ ['║', cursor t, '║']
                 ++ intersperse '│' (trail (right t))
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

next :: Machine -> Input -> PartFun
next tm (x_state, x_symbol) = head . filter 
                    (\f -> input f `compare` (x_state, x_symbol)) $ partFun tm
        where 
            compare (state, '*') (state', _) = state == state'
            compare a b                      = a == b 

update :: Tape -> Action -> Tape
update tape (_, '*', direction) = tape `moveCursor` direction
update tape (_, c, direction)   = tape { cursor = c } `moveCursor` direction 

compute :: Machine -> State -> Tape -> [(PartFun, Tape)]
compute tm state tape 
    | tm `finished` state = []
    | otherwise           = do
        let symbol = cursor tape
            fun = tm `next` (state, symbol)
            (state', _, _) = action fun
            tape' = tape `update` (action fun)
        (fun, tape') : compute tm state' tape'


