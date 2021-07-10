{-# OPTIONS_GHC -Wall #-}
module Hole
  ( isInsideHole
  ) where

import Data.Monoid
import Data.Ratio
import qualified Parser as P

isInsideHole :: P.Point -> P.Hole -> Bool
isInsideHole p h =
  case getAp m of
    Nothing -> True
    Just cp -> odd (getSum cp :: Int)
  where
    m = mconcat
      [ if isOn p e then
          Ap Nothing
        else if isCrossing p e then
          Ap (Just 1)
        else
          Ap (Just 0)
      | e <- zip h (tail h ++ [head h])
      ]

isOn :: P.Point -> (P.Point, P.Point) -> Bool
isOn (P.Point x y) (P.Point x1 y1, P.Point x2 y2)
  | not (y1 <= y && y <= y2) && not (y2 <= y && y <= y1) = False
  | y1 == y2 = x1 <= x && x <= x2
  | otherwise = (fromIntegral x :: Rational) == fromIntegral x1 + fromIntegral (y - y1) * (fromIntegral (x2 - x1) % fromIntegral (y2 - y1))

-- https://www.nttpc.co.jp/technology/number_algorithm.html
isCrossing :: P.Point -> (P.Point, P.Point) -> Bool
isCrossing (P.Point x y) (P.Point x1 y1, P.Point x2 y2) =
  ((y1 <= y && y < y2) || (y2 <= y && y < y1)) &&
  (fromIntegral x :: Rational) < fromIntegral x1 + fromIntegral (y - y1) * (fromIntegral (x2 - x1) % fromIntegral (y2 - y1))


lambdaman :: P.Hole
lambdaman = [P.Point x y | (x,y) <- [(55, 80), (65, 95), (95, 95), (35, 5), (5, 5), (35, 50), (5, 95), (35, 95), (45, 80)]]

testPos1 = and [isInsideHole p lambdaman | p <- lambdaman]

testPos2 = and
  [ isInsideHole (P.Point 35 5) lambdaman
  , isInsideHole (P.Point 35 10) lambdaman
  , isInsideHole (P.Point 35 50) lambdaman
  , isInsideHole (P.Point 40 50) lambdaman
  , isInsideHole (P.Point 35 75) lambdaman
  , isInsideHole (P.Point 35 80) lambdaman
  , isInsideHole (P.Point 35 85) lambdaman
  , isInsideHole (P.Point 35 90) lambdaman
  , isInsideHole (P.Point 35 95) lambdaman

  , isInsideHole (P.Point 6 6) lambdaman
  , not $ isInsideHole (P.Point 34 48) lambdaman
  ]

-- Lambdaman
testNeg = and
  [ not $ isInsideHole (P.Point 4 4) lambdaman
  , not $ isInsideHole (P.Point 5 4) lambdaman
  , not $ isInsideHole (P.Point 35 4) lambdaman
  , not $ isInsideHole (P.Point 45 4) lambdaman
  , not $ isInsideHole (P.Point 55 4) lambdaman
  , not $ isInsideHole (P.Point 65 4) lambdaman
  , not $ isInsideHole (P.Point 95 4) lambdaman

  , not $ isInsideHole (P.Point 4 100) lambdaman
  , not $ isInsideHole (P.Point 5 100) lambdaman
  , not $ isInsideHole (P.Point 35 100) lambdaman
  , not $ isInsideHole (P.Point 45 100) lambdaman
  , not $ isInsideHole (P.Point 55 100) lambdaman
  , not $ isInsideHole (P.Point 65 100) lambdaman
  , not $ isInsideHole (P.Point 95 100) lambdaman

  , not $ isInsideHole (P.Point 4 4) lambdaman
  , not $ isInsideHole (P.Point 4 5) lambdaman
  , not $ isInsideHole (P.Point 4 40) lambdaman
  , not $ isInsideHole (P.Point 4 50) lambdaman
  , not $ isInsideHole (P.Point 4 60) lambdaman
  , not $ isInsideHole (P.Point 4 70) lambdaman
  , not $ isInsideHole (P.Point 4 80) lambdaman
  , not $ isInsideHole (P.Point 4 90) lambdaman
  , not $ isInsideHole (P.Point 4 95) lambdaman
  , not $ isInsideHole (P.Point 4 100) lambdaman

  , not $ isInsideHole (P.Point 100 4) lambdaman
  , not $ isInsideHole (P.Point 100 5) lambdaman
  , not $ isInsideHole (P.Point 100 40) lambdaman
  , not $ isInsideHole (P.Point 100 50) lambdaman
  , not $ isInsideHole (P.Point 100 60) lambdaman
  , not $ isInsideHole (P.Point 100 70) lambdaman
  , not $ isInsideHole (P.Point 100 80) lambdaman
  , not $ isInsideHole (P.Point 100 90) lambdaman
  , not $ isInsideHole (P.Point 100 95) lambdaman
  , not $ isInsideHole (P.Point 100 100) lambdaman

  , not $ isInsideHole (P.Point 6 7) lambdaman
  , not $ isInsideHole (P.Point 34 49) lambdaman
  , not $ isInsideHole (P.Point 34 50) lambdaman
  , not $ isInsideHole (P.Point 44 82) lambdaman  
  , not $ isInsideHole (P.Point 36 96) lambdaman
  , not $ isInsideHole (P.Point 36 6) lambdaman
  , not $ isInsideHole (P.Point 66 50) lambdaman
  ] 
