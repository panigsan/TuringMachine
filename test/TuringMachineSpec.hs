{-# LANGUAGE ScopedTypeVariables #-}


module TuringMachineSpec (spec) where

import  TuringMachine
import  Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  do
    describe "initTape" $ do
        it "init empty tape" $
            initTape "" ' ' `shouldBe` Tape {left="", cursor=' ', right="", blank=' '}
        it "init tape with string" $
            initTape "1010" ' ' `shouldBe` Tape {left="", cursor='1', right="010", blank=' '}

    describe "moveCursor" $ do
        let emptyTape = Tape {left="", cursor=' ', right="", blank=' '}
            tape1     = Tape {left="ba", cursor='c', right="de", blank=' '}
        it "move empty tape to the left" $
            moveCursor emptyTape L `shouldBe` Tape {left="", cursor=' ', right=" ", blank=' '}
        it "move empty tape to the right" $
            moveCursor emptyTape R `shouldBe` Tape {left=" ", cursor=' ', right="", blank=' '}
        it "move empty tape to Stay" $
            moveCursor emptyTape S `shouldBe` Tape {left="", cursor=' ', right="", blank=' '}
        it "move not empty tape to the left" $
            moveCursor tape1 L `shouldBe` Tape {left="a", cursor='b', right="cde", blank=' '}
        it "move not empty tape to the right" $
            moveCursor tape1 R `shouldBe` Tape {left="cba", cursor='d', right="e", blank=' '}
        it "move not empty tape to Stay" $
            moveCursor tape1 S `shouldBe` Tape {left="ba", cursor='c', right="de", blank=' '}

    describe "finished" $ do
        let tm1 = Machine {states=["q0","q1","q2","q3"], tapeAlphabet="", blankSymbol=' ', inputSymbol="", partFun=[], initialState="q0", finalStates=["q2","q3"]}
        it "does finish" $ do
            finished tm1 "q2" `shouldBe` True
            finished tm1 "q3" `shouldBe` True
        it "does not finishes" $ do
            finished tm1 "q0" `shouldBe` False
            finished tm1 "q1" `shouldBe` False
