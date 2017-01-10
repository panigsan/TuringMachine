{-# LANGUAGE ScopedTypeVariables #-}


module TuringMachineSpec (spec) where

import TuringMachine
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  do
    describe "initTape" $ do
        it "init empty tape" $
            initTape "" `shouldBe` Tape "" '_' ""--{left="", cursor=blank, right=""}
        it "init tape with string" $
            initTape "1010" `shouldBe` Tape "" '1' "010"--{left="", cursor='1', right="010"}
        it "replaces spaces with underscores" $ do
            initTape " 11" `shouldBe` Tape "" '_' "11"
            initTape "1 1 1" `shouldBe` Tape "" '1' "_1_1"

    describe "moveCursor" $ do
        let emptyTape = Tape "" '_' ""
            tape1     = Tape "ba" 'c' "de"
        it "move empty tape to the left" $
            moveCursor emptyTape L `shouldBe` Tape "" '_' "_"
        it "move empty tape to the right" $
            moveCursor emptyTape R `shouldBe` Tape "_" '_' ""
        it "move empty tape to Stay" $
            moveCursor emptyTape S `shouldBe` Tape "" '_' ""
        it "move not empty tape to the left" $
            moveCursor tape1 L `shouldBe` Tape "a" 'b' "cde"
        it "move not empty tape to the right" $
            moveCursor tape1 R `shouldBe` Tape "cba" 'd' "e"
        it "move not empty tape to Stay" $
            moveCursor tape1 S `shouldBe` Tape "ba" 'c' "de"

    describe "finished" $ do
        let tm1 = Machine {partFun=[], initialState="q0", finalStates=["q2","q3"]}
        it "does finish" $ do
            finished tm1 "q2" `shouldBe` True
            finished tm1 "q3" `shouldBe` True
        it "does not finishes" $ do
            finished tm1 "q0" `shouldBe` False
            finished tm1 "q1" `shouldBe` False

    describe "next" $ do
        let tm1 = Machine { partFun = [ PartFun ("q0", '0') ("q0", '0', R)
                                      , PartFun ("q0", '1') ("q1", '1', R)
                                      , PartFun ("q0", '*') ("q0", '*', L)
                                      , PartFun ("q1", '0') ("q2", '1', L)
                                      , PartFun ("q1", '1') ("q3", '0', L)
                                      ]
                          , initialState="q0", finalStates=["q2","q3"]
                          }
        it "get the right next" $ do
            tm1 `next` ("q0", '0') `shouldBe` Just (PartFun ("q0", '0') ("q0", '0', R))
            tm1 `next` ("q0", '1') `shouldBe` Just (PartFun ("q0", '1') ("q1", '1', R))
            tm1 `next` ("q1", '0') `shouldBe` Just (PartFun ("q1", '0') ("q2", '1', L))
            tm1 `next` ("q1", '1') `shouldBe` Just (PartFun ("q1", '1') ("q3", '0', L))
            tm1 `next` ("q0", 'k') `shouldBe` Just (PartFun ("q0", '*') ("q0", '*', L))
        it "does not find the succesive action" $ do
            tm1 `next` ("q1", 'k') `shouldBe` Nothing
            tm1 `next` ("q2", '0') `shouldBe` Nothing

    describe "update" $ do
        let tape = Tape "ba" 'c' "de"
        it "updates with the correct character" $ do
            tape `update` 'k' `shouldBe` Tape "ba" 'k' "de"
            tape `update` 'X' `shouldBe` Tape "ba" 'X' "de"
        it "updates with * character" $ do
            tape `update` '*' `shouldBe` Tape "ba" 'c' "de"

    describe "compute" $ do
        let q0_ = PartFun ("q0", '_') ("q1", '_', L)
            q0' = PartFun ("q0", '*') ("q0", '*', R)
            q10 = PartFun ("q1", '0') ("q2", '1', S)
            q11 = PartFun ("q1", '1') ("q1", '0', L)
            tm = Machine { initialState = "q0"
                         , finalStates  = ["q2"]
                         , partFun = [ q0_, q0'
                                     , q10, q11]}
            tape = Tape ""  '0' "11"
        it "computes correctly binary +1" $ do
            compute tm "q0" tape `shouldBe`
                [ (Just q0', Tape "" '0' "11")
                , (Just q0', Tape "0" '1' "1")
                , (Just q0', Tape "10" '1' "")
                , (Just q0_, Tape "110" '_' "")
                , (Just q11, Tape "10" '1' "_")
                , (Just q11, Tape "10" '0' "_")
                , (Just q11, Tape "0" '1' "0_")
                , (Just q11, Tape "0" '0' "0_")
                , (Just q10, Tape "" '0' "00_")
                , (Just q10, Tape "" '1' "00_")
                , (Nothing, Tape "" '1' "00_")
                ]
