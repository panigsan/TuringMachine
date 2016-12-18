{-# LANGUAGE ScopedTypeVariables #-}


module TuringMachineSpec (spec) where

import TuringMachine as Tm
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  do
    describe "initTape" $ do
        it "init empty tape" $
            initTape "" `shouldBe` Tape {left="", cursor=blank, right=""}
        it "init tape with string" $
            initTape "1010" `shouldBe` Tape {left="", cursor='1', right="010"}

    describe "moveCursor" $ do
        let emptyTape = Tape {left="", cursor=blank, right=""}
            tape1     = Tape {left="ba", cursor='c', right="de"}
        it "move empty tape to the left" $
            moveCursor emptyTape L `shouldBe` Tape {left="", cursor=blank, right=[blank]}
        it "move empty tape to the right" $
            moveCursor emptyTape R `shouldBe` Tape {left=[blank], cursor=blank, right=""}
        it "move empty tape to Stay" $
            moveCursor emptyTape S `shouldBe` Tape {left="", cursor=blank, right=""}
        it "move not empty tape to the left" $
            moveCursor tape1 L `shouldBe` Tape {left="a", cursor='b', right="cde"}
        it "move not empty tape to the right" $
            moveCursor tape1 R `shouldBe` Tape {left="cba", cursor='d', right="e"}
        it "move not empty tape to Stay" $
            moveCursor tape1 S `shouldBe` Tape {left="ba", cursor='c', right="de"}

    describe "finished" $ do
        let tm1 = Machine {partFun=[], initialState="q0", finalStates=["q2","q3"]}
        it "does finish" $ do
            finished tm1 "q2" `shouldBe` True
            finished tm1 "q3" `shouldBe` True
        it "does not finishes" $ do
            finished tm1 "q0" `shouldBe` False
            finished tm1 "q1" `shouldBe` False
    
    describe "next" $ do
        let tm1 = Machine { partFun = [ PartFun { Tm.input  = ("q0", '0') 
                                                , Tm.output = ("q0", '0', R)}
                                      , PartFun { Tm.input  = ("q0", '1')
                                                , Tm.output = ("q1", '1', R)}
                                      , PartFun { Tm.input  = ("q1", '0')
                                                , Tm.output = ("q2", '1', L)}
                                      , PartFun { Tm.input  = ("q1", '1')
                                                , Tm.output = ("q3", '0', L)}
                                      ]
                          , initialState="q0", finalStates=["q2","q3"]
        }
        it "get the right next" $ do
            tm1 `next` ("q0", '0') `shouldBe` ("q0", '0', R)
            tm1 `next` ("q0", '1') `shouldBe` ("q1", '1', R)
            tm1 `next` ("q1", '0') `shouldBe` ("q2", '1', L)
            tm1 `next` ("q1", '1') `shouldBe` ("q3", '0', L)
