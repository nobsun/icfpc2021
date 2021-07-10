{-# LANGUAGE DisambiguateRecordFields #-}
module Solver.SMT where

import qualified Data.Vector as V
import System.IO
import Text.Printf

import qualified Parser as P
import qualified Hole

solve :: P.Problem -> IO P.Pose
solve prob = do
  hPrintf stderr "#vertices = %d\n" (length vs)
  hPrintf stderr "#edges = %d\n" (length es)

  undefined

  where
    hole = P.hole prob
    fig@(P.Figure{ P.edges = es, P.vertices = vs' }) = P.figure prob
    vs = V.fromList vs'
    -- es = P.edges fig
    eps = P.epsilon prob

    innerPoints = Hole.innerPoints hole
