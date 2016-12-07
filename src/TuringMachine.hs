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
                  { states         :: [ State ]
                  , tapeAlphabet   :: [ Symbol ]
                  , blankSymbol    :: Symbol 
                  , inputSymbol    :: [ Symbol ]
                  , partFun        :: [ PartFun ]
                  , initialState   :: State
                  , finalStates    :: [ State]
                  }
    deriving (Show)

data Tape = Tape
            { left      :: [ Symbol ]
            , cursor    :: Symbol
            , right     :: [ Symbol ]
            , blank     :: Symbol
            }
    deriving (Show)


-- Initialize a new tape with the given string and the given blank symbol
initTape :: [ Symbol ] -> Symbol -> Tape
initTape (x:xs) b = Tape
                    { left    = []
                    , cursor  = x
                    , right   = xs
                    , blank   = b
                    }

--initPartFun :: State a => [ (a, Symbol), (a, Symbol, Direction) ] -> [PartFun]
initPartFun = map (\(x,y) -> PartFun x y)

-- Move cursor on the tape
moveCursor :: Tape -> Direction -> Tape

moveCursor t R = t { left   = cursor t : (left t)
                   , cursor = if null $ right t
                                  then blank t
                                  else head $ right t
                   , right  = if null $ right t
                                  then ""
                                  else tail $ right t
                   }

moveCursor t L = t { left    = if null $ left t
                                   then ""
                                   else tail $ left t
                    , cursor = if null $ left t
                                   then blank t
                                   else head $ left t
                    , right   = cursor t : (right t)
                    }
moveCursor t S = t

-- Conver the tape to a single string with Int visible symbols
fancyTape :: Tape -> Int -> String
fancyTape t x = (reverse $ trail $ left t)     ++
                [ '|', cursor t, '|' ]      ++ 
                trail (right t)
            where
                -- equal spaces on each side
                sides = (x - 1) `div` 2
                -- add or remove symbols in order to keep both sides of the same length
                trail text | (length text < sides) = text ++ replicate (sides - length text) ' '
                           | otherwise = take sides text
                

finished :: Machine -> State -> Bool
finished tm state = state `elem` (finalStates tm)

next :: Machine -> (State, Symbol) -> (State, Symbol, Direction)
next machine x = head $ map (output)
                                  (filter (\f -> input f == x) (partFun machine))

execute :: Machine -> Tape -> State -> IO()
execute tm tape state = do
    putStrLn $ show state ++ ":" ++ fancyTape tape 21
    if not $ tm `finished` state
    then
        let symbol = cursor tape
            (state', symbol', direction') = tm `next` (state, symbol)
            tape' = tape { cursor = symbol' } `moveCursor` direction'
        in execute tm tape' state'
    else
        putStrLn "finished"
    

