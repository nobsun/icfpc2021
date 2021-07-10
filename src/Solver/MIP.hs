{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Solver.MIP
  ( solve
  ) where

import Control.Monad
import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Data.Scientific
import Data.Semigroup
import Data.String
import Data.Time
import qualified Data.Vector as V
import System.IO
import Text.Printf

import Numeric.Optimization.MIP ((.==.), (.>=.), (.<=.), def)
import qualified Numeric.Optimization.MIP as MIP
import qualified Numeric.Optimization.MIP.Solver as MIP

import qualified Parser as P
import qualified Hole


data Mode
  = ModeInd
  | ModeBigM
  deriving (Show, Eq, Ord, Enum, Bounded)

solve :: P.Problem -> IO P.Pose
solve prob = do
  hPrintf stderr "#vertices = %d\n" (length vs)
  hPrintf stderr "#edges = %d\n" (length es)
  hPrintf stderr "#innerPoints = %d\n" (length innerPoints)
  hPrintf stderr "#eps = %d\n" eps

  let mode = ModeBigM

  let xs = Map.fromList [(i, fromString ("x" <> show i)) | (i, _) <- zip [(0::Int)..] (V.toList vs)]
      ys = Map.fromList [(i, fromString ("y" <> show i)) | (i, _) <- zip [(0::Int)..] (V.toList vs)]
      os =
        [ (e, [(off, fromString (printf "e_%d_%d" i j)) | (j, off) <- zip [(0::Int)..] (offsets eps d)])
        | (i, e@(P.Edge s t)) <- zip [(0::Int)..] es, let d = distance (vs V.! s) (vs V.! t)
        ]

  let (Min x_min, Max x_max, Min y_min, Max y_max) =
        sconcat $ fromList [(Min x, Max x, Min y, Max y) | P.Point x y <- innerPoints]

  let prob :: MIP.Problem Scientific
      prob = def
        { MIP.constraints = concat
            [ [MIP.Expr [MIP.Term 1 [v] | (_off, v) <- zs] .==. 1 | (_e, zs) <- os]
            , concat
              [ case mode of
                  ModeInd ->
                    [ -- v=1 -> x2 - x1 = x_off
                      (MIP.Expr [MIP.Term 1 [xs Map.! t], MIP.Term (-1) [xs Map.! s]] .==. fromIntegral x_off)
                      { MIP.constrIndicator = Just (v, 1)}
                    , -- v=1 -> y2 - y1 = y_off
                      (MIP.Expr [MIP.Term 1 [ys Map.! t], MIP.Term (-1) [ys Map.! s]] .==. fromIntegral y_off)
                      { MIP.constrIndicator = Just (v, 1)}
                    ]
                  ModeBigM ->
                    [ -- M * (1 - v) + x2 - x1 >= x_off
                      MIP.Expr [MIP.Term (- m) [v], MIP.Term 1 [xs Map.! t], MIP.Term (-1) [xs Map.! s]] .>=. fromIntegral x_off - MIP.constExpr m
                    , -- -M * (1 - v) + x2 - x1 <= x_off
                      MIP.Expr [MIP.Term m [v], MIP.Term 1 [xs Map.! t], MIP.Term (-1) [xs Map.! s]] .<=. fromIntegral x_off + MIP.constExpr m
                    , -- M * (1 - v) + y2 - y1 >= y_off
                      MIP.Expr [MIP.Term (- m) [v], MIP.Term 1 [ys Map.! t], MIP.Term (-1) [ys Map.! s]] .>=. fromIntegral y_off - MIP.constExpr m
                    , -- -M * (1 - v) + y2 - y1 <= y_off
                      MIP.Expr [MIP.Term m [v], MIP.Term 1 [ys Map.! t], MIP.Term (-1) [ys Map.! s]] .<=. fromIntegral y_off + MIP.constExpr m
                    ]
              | (P.Edge s t, zs) <- os, ((x_off, y_off), v) <- zs
              , let m = 10000
              ]
            ]
        , MIP.varType = Map.unions
            [ Map.fromList [(x, MIP.IntegerVariable) | x <- Map.elems xs ++ Map.elems ys]
            , Map.fromList [(v, MIP.IntegerVariable) | (_e, zs) <- os, (_off, v) <- zs]
            ]
        , MIP.varBounds = Map.unions
            [ Map.fromList [(x, (MIP.Finite (fromIntegral x_min), MIP.Finite (fromIntegral x_max))) | x <- Map.elems xs]
            , Map.fromList [(y, (MIP.Finite (fromIntegral y_min), MIP.Finite (fromIntegral y_max))) | y <- Map.elems ys]
            , Map.fromList [(v, (MIP.Finite 0, MIP.Finite 1)) | (_e, zs) <- os, (_off, v) <- zs]
            ]          
        }

  MIP.writeFile def "test.lp" prob

  let solver :: MIP.CBC
      solver = def
      l s = do
        tm <- getCurrentTime
        putStrLn $ show tm ++ "> " ++ s
        hFlush stdout
      opt = def
        { MIP.solveLogger = l
        , MIP.solveErrorLogger = l
        -- , MIP.solveTimeLimit = Just 60.0
        }

  sol <- MIP.solve solver opt prob

  return undefined

  where
    hole = P.hole prob
    P.Figure{ P.edges = es, P.vertices = vs' } = P.figure prob
    vs = V.fromList vs'
    eps = P.epsilon prob

    innerPoints = Hole.innerPoints hole


distance :: P.Point -> P.Point -> Int
distance (P.Point x1 y1) (P.Point x2 y2) = (x2 - x1)^(2::Int) + (y2 - y1)^(2::Int)


offsets :: Int -> Int -> [(Int, Int)]
offsets eps d = [(x,y) | x <- [- z .. z], y <- [-z .. z], let d' = x*x + y*y, min_d <= d' && d' <= max_d]
  where
    min_d = d + ceiling (- fromIntegral d * fromIntegral eps / 1000000 :: Rational)
    max_d = d + floor (fromIntegral d * fromIntegral eps / 1000000 :: Rational)
    z = ceiling (sqrt (fromIntegral max_d + 0.0001 :: Double))
