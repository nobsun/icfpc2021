{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -Wno-unused-top-binds #-}
module Graph
  ( GridPoint
  , GSegment
  , GVertex
  , GEdge
  , GHole
  , GFigure
  , GProblem
  , GEpsilon
  , ghole
  , gfigure
  , gepsilon
  ) where

import Data.Aeson
import Data.Maybe
import qualified Data.Graph.Inductive as G
import qualified Parser as P

-- | 格子点
type GridPoint = (Int, Int)

pointToGridPoint :: P.Point -> GridPoint
pointToGridPoint = \ case
  P.Point x y -> (x, y)

-- | 線分
type GSegment   = (GridPoint, GridPoint)

-- | 頂点
type GVertex = G.LNode GridPoint

-- | 辺
type GEdge   = G.LEdge Dist

pedgeToGedge :: P.Edge -> GEdge
pedgeToGedge = \ case
  P.Edge s e -> (s,e,0)

-- | 穴
type GHole = G.Gr GridPoint ()

-- | 人形
type GFigure = G.Gr GridPoint Dist

{- |
>>> :set -XOverloadedStrings
>>> Just fig = decode "{\"edges\":[[0,1],[0,2],[1,3],[2,3],[2,4],[3,4]],\"vertices\":[[0,20],[20,0],[20,40],[40,20],[49,45]]}" :: Maybe P.Figure
>>> pfigToGfig fig
mkGraph [(0,(0,20)),(1,(20,0)),(2,(20,40)),(3,(40,20)),(4,(49,45))] [(0,1,800),(0,2,800),(1,3,800),(2,3,800),(2,4,866),(3,4,706)]
-}
pfigToGfig :: P.Figure -> GFigure
pfigToGfig = \ case
  P.Figure es vs -> mkFigure (map pointToGridPoint vs) es 

-- | ε
type GEpsilon = Int
-- | 問題
type GProblem = (GHole, GFigure, GEpsilon)

ghole :: GProblem -> GHole
ghole (h,_,_) = h
gfigure :: GProblem -> GFigure
gfigure (_,f,_) = f
gepsilon :: GProblem -> GEpsilon
gepsilon (_,_,e) = e

mkHole :: [GridPoint] -> GHole
mkHole vs = G.mkGraph ns ues
  where
    ns  = zip [0..] vs
    n   = length vs
    uns = cycle [0 .. n-1]
    ues = take n (zip3 uns (tail uns) (repeat ()))

mkFigure :: [GridPoint] -> P.Edges -> GFigure
mkFigure vs es = G.emap dist $ G.mkGraph (G.labNodes g) 
               $ map (\ e@(s,t,_) -> (s,t,segment g e)) (G.labEdges g)
  where
    g :: G.Gr GridPoint Dist
    g = G.mkGraph ns les
    ns  = zip [0 ..] vs
    les = map pedgeToGedge es

-- | 辺から線分へ
segment :: G.Gr GridPoint Dist -> GEdge -> GSegment
segment g e = case e of
  (m, n, _) -> (fromJust (G.lab g m), fromJust (G.lab g n))

{- --
mkGraph (labNodes g) $ map (segment g) (labEdges g)
-- -}

-- | 頂点の座標
coord :: GVertex -> GridPoint
coord = snd

-- | 辺の数
size :: G.Gr GridPoint () -> Int
size = G.size

-- | 頂点の数
order :: G.Gr GridPoint () -> Int
order = G.order

-- | 自乗距離
dist :: GSegment -> Dist
dist (a, b) = ab · ab
  where
    ab = b − a

(·) :: (Int, Int) -> (Int, Int) -> Int
(xa, ya) · (xb, yb) = xa * xb + ya * yb

type Dist = Int

(−) :: GridPoint -> GridPoint -> GridPoint
(xa, ya) − (xb, yb) = (xb - xa, yb - ya)