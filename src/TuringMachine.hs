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

-- Simple TM
fun :: PartFun
fun 0 '0' = (0, '1', R)
fun 0 '1' = (0, '0', R)
fun 0 ' ' = (1, ' ', R)

tm  = Machine 
     { states       = [0, 1]
     , tapeAlphabet = ['0', '1', ' ']
     , blankSymbol  = ' '
     , inputSymbol  = ['0', '1']
     , partFun      = fun
     , initialState = 0
     , finalStates  = [1]
     }

printFun :: PartFun -> String
printFun (state symbol (a, b, c)) = "aa"


