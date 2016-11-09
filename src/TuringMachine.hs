module TuringMachine where

type State      = Int
type AlphaSymb  = Int       -- what I can read
type BlankSymb  = AlphaSymb
type InputSymb  = AlphaSymb -- what I can write
data Direction  = Left | Right
type PartFun    = State -> AlphaSymb -> (State, InputSymb, Direction)


f :: Int -> Int
f a = a + 1

