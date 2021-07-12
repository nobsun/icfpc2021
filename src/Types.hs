module Types where

type G2DVector = (Int, Int)
type GridPoint = G2DVector
type GSegment  = (GridPoint, GridPoint)

type GDist = Int

-----

-- TwoDim
type Vec a = (a, a)

-----

-- Parser
data BonusType
  = GLOBALIST
  | BREAK_A_LEG
  | WALLHACK
  | SUPERFLEX
  deriving (Show, Eq)

-----
