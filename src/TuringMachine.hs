
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



finished :: Machine -> State -> Bool
finished tm state = state `elem` (finalStates tm)

next :: Machine -> Input -> PartFun
next tm (x_state, x_symbol) = head . filter
                    (\f -> input f `compare` (x_state, x_symbol)) $ partFun tm
        where
            compare (state, '*') (state', _) = state == state'
            compare a b                      = a == b

update :: Tape -> Char -> Tape
update tape '*' = tape
update tape c   = tape { cursor = c }

compute :: Machine -> State -> Tape -> [(Maybe PartFun, Tape)]
compute tm state tape
    | tm `finished` state = [(Nothing, tape)]
    | otherwise           = do
        let symbol = cursor tape
            fun = tm `next` (state, symbol)
            (state', c', direction') = action fun

            tape' = tape `update` c' -- tape in which the character has been added
            tape'' = tape' `moveCursor` direction'
        if tape == tape' then -- add the intermediary step only if the tape is different than the previous one
            [(Just fun, tape)] ++  compute tm state' tape''
        else
            [(Just fun, tape), (Just fun, tape')] ++  compute tm state' tape''
