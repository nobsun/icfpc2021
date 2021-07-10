{-# LANGUAGE MultiWayIf #-}
module Segment
  ( intersect
  , dist
  , (×)
  , (·)
  ) where

import Prelude hiding (pi)
import Graph ( GSegment, GridPoint )

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

-- | 外積
(×) :: (Int, Int) -> (Int, Int) -> Int
(xa, ya) × (xb, yb) = xa * yb - xb * ya

-- | 方向ベクトル
(−) :: GridPoint -> GridPoint -> GridPoint
(xa, ya) − (xb, yb) = (xb - xa, yb - ya)

-- | i を要とした j と k の位置関係: 0 < なら ccw、< 0 なら cw、 zero なら 3点は同一直線上にある
direction :: GridPoint -> GridPoint -> GridPoint -> Int
direction pi pj pk = (pk − pi) × (pj − pi)

-- | 端点が他方の線分上にあるか
onSegment :: GridPoint -> GridPoint -> GridPoint -> Bool
onSegment (xi, yi) (xj, yj) (xk, yk) = min xi xj <= xk && xk <= max xi xj && min yi yj <= yk && yk <= max yi yj

-- | 自乗距離
dist :: GSegment -> Int
dist (a, b) = ab · ab
  where
    ab = b − a
(·) :: (Int, Int) -> (Int, Int) -> Int
(xa, ya) · (xb, yb) = xa * xb + ya * yb
