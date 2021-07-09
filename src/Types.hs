module Types where

import Data.Maybe
import qualified Data.Graph.Inductive as G

-- | 格子点
type GridPoint = (Int, Int)
-- | 線分
type Segment   = (GridPoint, GridPoint)

-- | 頂点
type Vertex = G.LNode GridPoint
-- | 辺
type Edge   = G.UEdge

-- | 穴
type Hole = G.Gr GridPoint ()

-- | 形
type Figure = G.Gr GridPoint ()
-- | ε
type Epsilon = Int
-- | 問題
type Problem = (Hole, Figure, Epsilon)

hole :: Problem -> Hole
hole (h,_,_) = h
figure :: Problem -> Figure
figure (_,f,_) = f
epsilon :: Problem -> Epsilon
epsilon (_,_,e) = e

mkHole :: [GridPoint] -> Hole
mkHole vs = G.mkGraph ns ues
  where
    ns  = zip [0..] vs
    n   = length vs
    uns = cycle [0 .. n-1]
    ues = take n (zip3 uns (tail uns) (repeat ()))

mkFigure :: [GridPoint] -> [(Int, Int)] -> Figure
mkFigure vs es = G.mkGraph ns ues
  where
    ns  = zip [0 ..] vs
    ues = map (`G.toLEdge` ()) es

-- | 辺から線分へ
segment :: G.Gr GridPoint () -> Edge -> Segment
segment g e = case e of
  (m, n, _) -> (fromJust (G.lab g m), fromJust (G.lab g n))

-- | 頂点の座標
coord :: Vertex -> GridPoint
coord = snd

-- | 辺の数
size :: G.Gr GridPoint () -> Int
size = G.size

-- | 頂点の数
order :: G.Gr GridPoint () -> Int
order = G.order