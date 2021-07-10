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
