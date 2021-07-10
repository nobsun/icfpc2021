module Score
  ( sqrDistance
  , dislike
  ) where

import Graph

-- | squared distance
sqrDistance :: (GridPoint, GridPoint) -> Int
sqrDistance ((px, py), (qx, qy)) = (px - qx)^2 + (py - qy)^2

dislike :: ([GridPoint], [GridPoint]) -> Int
dislike (hole, pose) = sum [f h | h <- hole] -- map f hole
  where
    f h = minimum [sqrDistance (h, v) | v <- pose]
