{-# LANGUAGE ScopedTypeVariables #-}


module UtilSpec (spec) where

import TuringMachine
import Util
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "importTM" $ do
        it "does import a tm from a string" $ do
            let text = [ "Init = 0"
                       , "Final = 5"
                       , ""
                       , "# go to the end"
                       , "0 _ => 1 _ L"
                       , "0 * => 0 * R"
                       , ""
                       , "# first adder"
                       , "1 0 => 2 1 S"
                       , "1 _ => 2 1 S"
                       , "1 1 => 1 0 L"
                       ]
            importTM text `shouldBe`
                Just Machine { initialState = "0"
                             , finalStates  = ["5"]
                             , partFun =
                                 [ PartFun ("0", '_') ("1", '_', L)
                                 , PartFun ("0", '*') ("0", '*', R)
                                 , PartFun ("1", '0') ("2", '1', S)
                                 , PartFun ("1", '_') ("2", '1', S)
                                 , PartFun ("1", '1') ("1", '0', L)
                                 ]
                             }

        it "does not import a tm - initial state not valid" $ do
            let text = [ "Init ="
                       , "Final = 3"]
            importTM text `shouldBe` Nothing
        it "does not import a tm - final states not valid" $ do
            let text = [ "Init = 0"
                       , "Final ="]
            importTM text `shouldBe` Nothing

    describe "parseFinalStates" $ do
        it "parses single final state" $ do
            parseFinalStates ["Final", "=" ,"2"] `shouldBe`
                             ["2"]
        it "parses multiple final states" $ do
            parseFinalStates ["Final", "=" ,"2", "3", "4"] `shouldBe`
                             ["2", "3", "4"]

    describe "parseFun" $ do
        it "does parse a function" $ do
            parseFun ["0", "_", "=>", "1", "S", "L"] `shouldBe`
                Just (PartFun ("0", '_') ("1", 'S', L))
            parseFun ["1", "e", "=>", "2", "E", "R"] `shouldBe`
                Just (PartFun ("1", 'e') ("2", 'E', R))
        it "does not parse a function - input symbol not valid" $ do
            parseFun ["0", "AA", "=>", "1", "S", "L"] `shouldBe`
                Nothing
        it "does not parse a function - output symbol not valid" $ do
            parseFun ["0", "_", "=>", "1", "SS", "L"] `shouldBe`
                Nothing
        it "does not parse a function - direction not valid" $ do
            parseFun ["0", "_", "=>", "1", "S", "K"] `shouldBe`
                Nothing
        it "does not parse a function - not enough parameters" $ do
            parseFun [] `shouldBe` Nothing
            parseFun ["0", "_", "=>", "1", "S"] `shouldBe` Nothing

    describe "keepJust" $ do
        let f1 = PartFun ("0", '_') ("1", 'S', L)
            f2 = PartFun ("1", '1') ("0", '0', R)
            f3 = PartFun ("2", '2') ("2", '2', R)

        it "keep only just elements" $ do
            keepJust [] `shouldBe` []
            keepJust [Nothing] `shouldBe` []
            keepJust [Just f1] `shouldBe` [f1]
            keepJust [Just f1, Just f2, Just f3] `shouldBe`
                     [f1, f2, f3]
            keepJust [Just f1, Nothing, Just f3] `shouldBe`
                     [f1, f3]
