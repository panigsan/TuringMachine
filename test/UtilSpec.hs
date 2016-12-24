{-# LANGUAGE ScopedTypeVariables #-}


module UtilSpec (spec) where

import TuringMachine
import Util
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "importTM" $ do
        it "import a tm from a string" $ do
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
                Machine { initialState = "0"
                        , finalStates  = ["5"]
                        , partFun = 
                            [ PartFun ("0", '_') ("1", '_', L)
                            , PartFun ("0", '*') ("0", '*', R)
                            , PartFun ("1", '0') ("2", '1', S)
                            , PartFun ("1", '_') ("2", '1', S)
                            , PartFun ("1", '1') ("1", '0', L)
                            ]
                        }
                       
    describe "parseFinalStates" $ do
        it "parses single final state" $ do
            parseFinalStates "Final = 2" `shouldBe` ["2"]
        it "parses multiple final states" $ do
            parseFinalStates "Final = 2 3 4" `shouldBe` ["2", "3", "4"]

    describe "parseFun" $ do
        it "parses a function" $ do
            parseFun ["0", "_", "=>", "1", "S", "L"] `shouldBe`
                PartFun ("0", '_') ("1", 'S', L)
            parseFun ["1", "e", "=>", "2", "E", "R"] `shouldBe`
                PartFun ("1", 'e') ("2", 'E', R)
