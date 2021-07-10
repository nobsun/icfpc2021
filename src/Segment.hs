{-# LANGUAGE MultiWayIf #-}
module Segment
  ( intersect
  ) where

import Graph

{- | 交差判定
>>> a = (0,0)
>>> b = (6,8)
>>> c = (0,8)
>>> d = (6,0)
>>> intersect (a,b) (c,d)
True
>>> b = (2,2)
>>> intersect (a,b) (c,d)
False
>>> b = (3,4)
>>> intersect (a,b) (c,d)
True
-}
intersect :: GSegment -> GSegment -> Bool
intersect (p1, p2) (p3, p4)
  = if
    | d1 * d2 < 0 && d3 * d4 < 0    -> True
    | d1 == 0 && onSegment p3 p4 p1 -> True
    | d2 == 0 && onSegment p3 p4 p2 -> True
    | d3 == 0 && onSegment p1 p2 p3 -> True
    | d4 == 0 && onSegment p1 p2 p4 -> True
    | otherwise                     -> False
  where
    d1 = direction p3 p4 p1
    d2 = direction p3 p4 p2
    d3 = direction p1 p2 p3
    d4 = direction p1 p2 p4


(×) :: (Int, Int) -> (Int, Int) -> Int
(xa, ya) × (xb, yb) = xa * yb - xb * ya

(−) :: GridPoint -> GridPoint -> GridPoint
(xa, ya) − (xb, yb) = (xb - xa, yb - ya)

direction :: GridPoint -> GridPoint -> GridPoint -> Int
direction pi pj pk = (pk − pi) × (pj − pi)

onSegment :: GridPoint -> GridPoint -> GridPoint -> Bool
onSegment (xi, yi) (xj, yj) (xk, yk) = min xi xj <= xk && xk <= max xi xj && min yi yj <= yk && yk <= max yi yj
