module TuringMachine where

type State      = Int
type Symbol     = Char
data Direction  = L | R
    deriving (Show)
type PartFun    = State -> Symbol -> (State, Symbol, Direction)

data Machine = Machine 
               { states         :: [ State ]
               , tapeAlphabet   :: [ Symbol ]
               , blankSymbol    :: Symbol 
               , inputSymbol    :: [ Symbol ]
               , partFun        :: PartFun
               , initialState   :: State
               , finalStates    :: [ State]
               }
    --deriving (Show)

finished :: Machine -> State -> Bool
finished tm state = state `elem` (finalStates tm)

execute :: Machine -> State -> IO()
execute tm state = do
    putStr "Symbol on tape: "
    symbol <- getChar :: IO Char
    putStrLn ""

    let (state', symbol', direction') = (partFun tm) state symbol
    putStrLn $ show $ (state', symbol', direction')
    
    if not $ finished tm state'
      then execute tm state'
      else putStrLn "ended"



-- Simple TM
fun :: PartFun
fun 0 '0' = (0, '1', R)
fun 0 '1' = (0, '0', R)
fun 0 ' ' = (1, ' ', R)

tm1  = Machine 
     { states       = [0, 1]
     , tapeAlphabet = ['0', '1', ' ']
     , blankSymbol  = ' '
     , inputSymbol  = ['0', '1']
     , partFun      = fun
     , initialState = 0
     , finalStates  = [1]
     }


