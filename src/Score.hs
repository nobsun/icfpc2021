module Score
  ( sqrDistance
  , dislike
  , tolerant
  ) where

import Graph

-- | squared distance
sqrDistance :: (GridPoint, GridPoint) -> Int
sqrDistance ((px, py), (qx, qy)) = (px - qx)^2 + (py - qy)^2

dislike :: ([GridPoint], [GridPoint]) -> Int
dislike (hole, pose) = sum [f h | h <- hole] -- map f hole
  where
    f h = minimum [sqrDistance (h, v) | v <- pose]

tolerant :: Int                    -- ^ epsilon
         -> (GridPoint, GridPoint) -- ^ before
         -> (GridPoint, GridPoint) -- ^ after
         -> Bool                   -- ^ result
tolerant eps (vi, vj) (vi', vj')
  | d' > d = (10^6 + eps) * d >= 10^6 * d'
  | d' < d = (10^6 - eps) * d <= 10^6 * d'
  | otherwise = True
  where
    d  = sqrDistance (vi,  vj )
    d' = sqrDistance (vi', vj')
