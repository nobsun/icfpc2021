module Score
  ( sqrDistance
  , dislike
  , tolerant
  , possibleLengthRange
  ) where

import Types

{- | squared distance
>>> sqrDistance ((30, 40), (35, 50))
125
>>> sqrDistance ((11, 42), (101, 18))
8676
-}
sqrDistance :: (GridPoint, GridPoint) -> Int
sqrDistance ((px, py), (qx, qy)) = (px - qx)^(2::Int) + (py - qy)^(2::Int)

{- | dislike
>>> dislike ([(10, 20)], [(20, 25)])
125
>>> dislike ([(10, 20), (15, 15), (20, 10)], [(20, 25), (15, 30), (20, 25)])
475
-}
dislike :: ([GridPoint], [GridPoint]) -> Int
dislike (hole, pose) = sum [f h | h <- hole]
  where
    f h = minimum [sqrDistance (h, v) | v <- pose]

{- | NOTE: 浮動小数点の計算を避けたけど Int の maxBound に納まるかは仕様を調べきれてない
--
--   abs (d'/d - 1) <= e/10^6
-- = abs ((d' - d)/d) <= e/10^6
-- = abs (d' - d) <= d*e/10^6 -- d は square distance なので常に正
-- = 10^6 * abs (d' - d) <= d * e
--
>>> tolerant 150000 ((10, 15), (20, 10)) ((11, 14), (21, 12))
False
>>> tolerant 150000 ((10, 15), (20, 10)) ((11, 14), (21, 11))
True
>>> tolerant 150000 ((10, 15), (20, 11)) ((11, 14), (21, 12))
True
>>> tolerant 150000 ((12, 15), (20, 11)) ((11, 14), (21, 12))
False
-}
tolerant :: Int                    -- ^ epsilon
         -> (GridPoint, GridPoint) -- ^ before
         -> (GridPoint, GridPoint) -- ^ after
         -> Bool                   -- ^ result
tolerant eps (vi, vj) (vi', vj')
  = c * abs (d' - d) <= d * eps
  where
    c = 10^(6::Int)
    d  = sqrDistance (vi,  vj )
    d' = sqrDistance (vi', vj')

{- |
>>> possibleLengthRange (10^(6::Int)) ((0,0),(3,4))
(0,50)
-}
possibleLengthRange
  :: Int                    -- ^ epsilon
  -> (GridPoint, GridPoint) -- ^ original edge
  -> (Int, Int)             -- ^ min and max length
possibleLengthRange eps (vi, vj) = (min', max')
 where
  c    = 10 ^ (6 :: Int)
  d    = sqrDistance (vi, vj)
  eps' = fromIntegral eps :: Rational
  max' = floor $ (1 + eps' / c) * toRational d
  min' = ceiling $ (1 - eps' / c) * toRational d

