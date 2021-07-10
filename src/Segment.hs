module Segment where

import Graph

intersect :: GSegment -> GSegment -> Bool
intersect ((xa,ya), (xb,yb)) ((xc,yc), (xd,yd)) 
  = oprod ab ac * oprod ab ad < 0 && oprod cd ca * oprod cd cb < 0
  where
    ab = (xb - xa, yb - ya)
    ac = (xc - xa, yc - ya)
    ad = (xd - xa, yd - ya)
    cd = (xd - xc, yd - yc)
    ca = (xa - xc, ya - yc)
    cb = (xc - xb, yc - yb)

oprod :: (Int, Int) -> (Int, Int) -> Int
oprod (xa, ya) (xb, yb) = xa * yb - xb * ya
