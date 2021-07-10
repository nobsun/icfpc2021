module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "-package aeson"
               , "-package fgl"
               , "-package vector"
               , "src/Parser.hs"
               , "src/Score.hs"
               , "src/Segment.hs"
               , "src/TwoDim.hs"
               ]
