module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "-package aeson"
               , "src/Parser.hs"
               ]
