{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , segment
  ) where

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

gedgeToPedge :: GEdge -> P.Edge
gedgeToPedge (s,e,_) = P.Edge s e

-- | 穴
type GHole = G.Gr GridPoint ()

-- | 人形
type GFigure = G.Gr GridPoint Dist
{- $setup
>>> import Data.Aeson
>>> :set -XOverloadedStrings
-}

{- |
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

-- | ポーズ

type GPose = (Maybe [P.BonusUse], [GridPoint], GFigure)

pposeToGpose :: (P.Figure, P.Pose) -> GPose
pposeToGpose (pfig, ppose) = case ppose of
  P.Pose bonus vs -> (bonus, map pointToGridPoint vs, pfigToGfig pfig)

{- |
>>> Just fig = decode "{\"edges\":[[0,1],[0,2],[1,3],[2,3],[2,4],[3,4]],\"vertices\":[[0,20],[20,0],[20,40],[40,20],[49,45]]}" :: Maybe P.Figure
>>> ofig = pfigToGfig fig
>>> checkPoseByEpsilon 1250 (undefined,[(15,0),(35,20),(0,24),(20,44),(30,19)],ofig)
[(1250,True,(800,800)),(1250,True,(800,801)),(1250,True,(800,801)),(1250,True,(800,800)),(1250,False,(866,925)),(1250,False,(706,725))]
>>> checkPoseByEpsilon 1250 (undefined,[(15,0),(35,20),(0,24),(20,44),(29,19)],ofig)
[(1250,True,(800,800)),(1250,True,(800,801)),(1250,True,(800,801)),(1250,True,(800,800)),(1250,True,(866,866)),(1250,True,(706,706))]
-}
checkPoseByEpsilon :: Int -> GPose -> [(Int, Bool, (Dist, Dist))]
checkPoseByEpsilon ε (_,gs,fig)
  = zipWith check oes pes'
    where
      oes = G.labEdges fig
      pes = map gedgeToPedge oes
      posefig = mkFigure gs pes
      pes' = G.labEdges posefig
      check (s,e,d) (s',e',d')
        = if | (s,e) /= (s',e')                    -> error "bug!!"
             | 10^(6::Int) * abs (d' - d) <= d * ε -> (ε, True, (d,d'))
             | otherwise                           -> (ε, False, (d,d'))


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

