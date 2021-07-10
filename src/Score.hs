module Score
  ( sqrDistance
  , dislike
  , tolerant
  ) where

import Graph

-- | squared distance
sqrDistance :: (GridPoint, GridPoint) -> Int
sqrDistance ((px, py), (qx, qy)) = (px - qx)^(2::Int) + (py - qy)^(2::Int)

dislike :: ([GridPoint], [GridPoint]) -> Int
dislike (hole, pose) = sum [f h | h <- hole]
  where
    f h = minimum [sqrDistance (h, v) | v <- pose]

-- | NOTE: 浮動小数点の計算を避けたけど Int の maxBound に納まるかは仕様を調べきれてない
--
--   abs (d'/d - 1) <= e/10^6
-- = abs ((d' - d)/d) <= e/10^6
-- = abs (d' - d) <= d*e/10^6 -- d は square distance なので常に正
--
-- (i) d' > d
--     d' - d <= d*e/10^6
--   = 10^6 * d' - 10^6 d <= d * e
--   = 10^6 * d' <= (10^6 + e) * d
--
-- (ii) d' < d
--     d - d' <= d*e/10^6
--   = 10^6 * d - 10^6 * d' <= d * e
--   = 10^6 * d - d * e <= 10^6 * d'
--   = (10^6 - e) * d <= 10^6 * d'
--
tolerant :: Int                    -- ^ epsilon
         -> (GridPoint, GridPoint) -- ^ before
         -> (GridPoint, GridPoint) -- ^ after
         -> Bool                   -- ^ result
tolerant eps (vi, vj) (vi', vj')
  = c * abs (d - d') <= d' * eps
  where
    c = 10^(6::Int)
    d  = sqrDistance (vi,  vj )
    d' = sqrDistance (vi', vj')
