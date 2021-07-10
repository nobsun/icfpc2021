module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-package aeson"
               , "src/Parser.hs"
               ]
