module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "-package aeson"
               , "-package fgl"
               , "src/Parser.hs"
               , "src/Segment"
               , "src/TwoDim"
               ]
