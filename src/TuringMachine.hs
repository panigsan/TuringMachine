-- | Data and functions to run a turing machine
module TuringMachine where
import Data.List

type State      = String
type Symbol     = Char
-- | Represents the input of partial function
type Input      = (State, Symbol)
-- | Represents the action/output of a partial function
type Action     = (State, Symbol, Direction)
data Direction  = L -- ^ Left
                | R -- ^ Right
                | S -- ^ Stay
    deriving (Show, Eq, Read)

-- | Represents a partial function
--
-- It consits of an input and and action/output
data PartFun    = PartFun
                  { input       :: Input
                  , action      :: Action
                  }
    deriving (Show, Eq)

-- | Represents a turing machine
--
-- It consists of multiple partial functions, an initial state and a
-- list of final states
data Machine    = Machine
                  { partFun        :: [ PartFun ]
                  , initialState   :: State
                  , finalStates    :: [ State]
                  }
    deriving (Show, Eq)

-- | Represents a tape used by the turing machine
--
-- The tape is seen as following: (reverse left) | cursor | right
--
-- Where the first element of left (x:_) is the closest element to the cursor
data Tape = Tape
            { left      :: [ Symbol ]
            , cursor    :: Symbol
            , right     :: [ Symbol ]
            }
    deriving (Show, Eq)

-- | The space is represented by the underscore. It allows the using of
-- spaces (underscores) in the definition of a turing machine (file)
blank = '_' :: Symbol

-- | Initializes a new tape with the given string. The first character of then
-- string will be the cursor, the rest will be the right side.
--
-- !! All spaces will be replaced by an underscore !!
initTape :: [ Symbol ] -> Tape
initTape []       = Tape
                    { left    = []
                    , cursor  = blank
                    , right   = []
                    }
initTape (x:xs)   = Tape
                    { left    = []
                    , cursor  = repl x
                    , right   = map repl xs
                    }
    where
        repl ' ' = blank
        repl x   = x

-- | Moves cursor on the tape
moveCursor :: Tape      -- ^ Tape to move
           -> Direction -- ^ Direction to move the cursor
           -> Tape      -- ^ Resulting tape

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

-- | Returns True if the given state is a final state
finished :: Machine -- ^ Turing machine
         -> State   -- ^ State to check
         -> Bool    -- ^ True if finished, False if it has to continue
finished tm state = state `elem` (finalStates tm)

-- | Returns the partial function in line with the given input. The symbol '*'
-- is considered as 'any value'
next :: Machine       -- ^ Turing Machine
     -> Input         -- ^ Current input (state and cursor)
     -> Maybe PartFun -- ^ Returing partial function
next tm x_input | null selection = Nothing
                | otherwise      = Just (head selection)
        where
            selection = filter (\f -> input f `compare` x_input) $ partFun tm
            compare (state, '*') (state', _) = state == state'
            compare a b                      = a == b

-- | Updates the cursor with the new character. In case the symbol '*' is given,
-- it does not change the tape
update :: Tape -- ^ Tape to update
       -> Char -- ^ Characther or '*' symbol
       -> Tape -- ^ Updated tape
update tape '*' = tape
update tape c   = tape { cursor = c }

-- | Executes the turing machine. It returns all the used partial function with
-- the relative tape.
-- If an action consists of changing the value of the cursor AND of moving the
-- cursor, then two elements in the returing list will be added. This is quite
-- useful when the results are printed on the screen because it shows the two
-- steps (changing value and moving) separated.
compute :: Machine -- ^ Turing Machine
        -> State   -- ^ Current state
        -> Tape    -- ^ Current tape
        -> [(Maybe PartFun, Tape)] -- ^ Used partial functions and tapes
compute tm state tape
    | tm `finished` state = [(Nothing, tape)]
    | mfun == Nothing     = [(Nothing, tape)]
    | otherwise           = do
        let (Just fun) = mfun
            (state', c', direction') = action fun

            tape' = tape `update` c' -- tape in which the character has been added
            tape'' = tape' `moveCursor` direction'
        -- add the intermediary step only if two tapes are different
        if tape == tape' then
            [(Just fun, tape)] ++  compute tm state' tape''
        else
            [(Just fun, tape), (Just fun, tape')] ++  compute tm state' tape''

        where symbol = cursor tape
              mfun = tm `next` (state, symbol)
