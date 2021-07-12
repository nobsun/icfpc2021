{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Graph.Inductive
import Data.GraphViz
import Bonus
import qualified Parser as P

main :: IO ()
main = do
  { bdeps <- getBonusDeps path numOfProb
  ; let viz = graphToDot graphvizParams (gmap phi bdeps) :: DotGraph Int
  ; p <- runGraphviz viz Png "./bonusdeps.png"
  ; putStrLn $ "Output filepath: " ++ p
  }

phi :: Context () P.BonusDef -> Context String String
phi (froms, node, lab, tos) = (map conv froms, node, "", map conv tos)
  where
    conv (b,n) = (show (P.bonus b), n)
                         
path :: FilePath
path = "data/problems"

numOfProb :: Int
numOfProb = 132

graphvizParams :: GraphvizParams Int String String () String
graphvizParams = defaultParams
