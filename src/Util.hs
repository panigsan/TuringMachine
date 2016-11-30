{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Util where

import TuringMachine
import System.IO

importTM :: String -> IO ()
importTM x = do
    contents <- readFile x
    let actions = lines contents
        
    return ()

parseFun :: String -> String
parseFun x = do
    let values = words
        {-fun = PartFun 
              { input = (values !! 0, values !! 1)
              , output = (values !! 3, values !! 4, values !! 5)
              -}
    "ciao"
