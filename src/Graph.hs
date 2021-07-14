{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -Wno-unused-top-binds #-}
module Graph where

import Data.Maybe
import qualified Data.Graph.Inductive as G
import qualified Parser as P
import Segment
import Types

-- | 格子点

pointToGridPoint :: P.Point -> GridPoint
pointToGridPoint = \ case
  P.Point x y -> (x, y)

-- | 線分
-- type GSegment   = (GridPoint, GridPoint)

-- | 頂点
type GVertex = G.LNode GridPoint

-- | 辺
type GEdge   = G.LEdge GDist

pedgeToGedge :: P.Edge -> G.UEdge
pedgeToGedge = \ case
  P.Edge s e -> (s,e,())

gedgeToPedge :: G.LEdge a -> P.Edge
gedgeToPedge (s,e,_) = P.Edge s e


-- | グラフ
type GGraph = G.Gr GridPoint GSegment

mkGGraph :: [GridPoint] -> [G.UEdge] -> GGraph
mkGGraph vs es = G.mkGraph ns ses
  where
    g :: G.Gr GridPoint ()
    g = G.mkGraph (zip [0..] vs) es
    ns = zip [0..] vs
    ses :: [G.LEdge GSegment]
    ses = map phi les
    phi :: G.LEdge () -> G.LEdge GSegment
    phi edg@(s,e,()) = (s,e, segment g edg)
    les :: [G.LEdge ()]
    les = G.labEdges g

-- | 穴
type GHole = GGraph

pholeToGHole :: P.Hole -> GHole
pholeToGHole = mkHole . map pointToGridPoint

mkHole :: [GridPoint] -> GHole
mkHole vs = mkGGraph vs es
  where
    vs' = zipWith const [0..] vs
    es  = zipWith phi vs' (tail (cycle vs'))
    phi s e = (s,e,())

-- | 人形
type GFigure = G.Gr GridPoint GSegment
{- $setup
>>> import Data.Aeson
>>> :set -XOverloadedStrings
-}

{- |
>>> Just fig = decode "{\"edges\":[[0,1],[0,2],[1,3],[2,3],[2,4],[3,4]],\"vertices\":[[0,20],[20,0],[20,40],[40,20],[49,45]]}" :: Maybe P.Figure
>>> gfig = pfigToGfig fig
>>> gfig
mkGraph [(0,(0,20)),(1,(20,0)),(2,(20,40)),(3,(40,20)),(4,(49,45))] [(0,1,((0,20),(20,0))),(0,2,((0,20),(20,40))),(1,3,((20,0),(40,20))),(2,3,((20,40),(40,20))),(2,4,((20,40),(49,45))),(3,4,((40,20),(49,45)))]
>>> G.emap dist gfig
mkGraph [(0,(0,20)),(1,(20,0)),(2,(20,40)),(3,(40,20)),(4,(49,45))] [(0,1,800),(0,2,800),(1,3,800),(2,3,800),(2,4,866),(3,4,706)]
-}
pfigToGfig :: P.Figure -> GFigure
pfigToGfig = \ case
  P.Figure es vs -> mkFigure (map pointToGridPoint vs) es 

-- | ε
type GEpsilon = Int

-- | 問題
type GProblem = (Maybe [P.BonusDef], GHole, GFigure, GEpsilon)

pprobToGProb :: P.Problem -> GProblem
pprobToGProb = \ case
  P.Problem bonus hole fig eps -> (bonus, pholeToGHole hole, pfigToGfig fig, eps)

gbonus :: GProblem -> Maybe [P.BonusDef]
gbonus (b,_,_,_) = b
ghole :: GProblem -> GHole
ghole (_,h,_,_) = h
gfigure :: GProblem -> GFigure
gfigure (_,_,f,_) = f
gepsilon :: GProblem -> GEpsilon
gepsilon (_,_,_,e) = e

mkFigure :: [GridPoint] -> P.Edges -> GFigure
mkFigure vs es = mkGGraph vs (map pedgeToGedge es)


-- | 辺から線分へ
segment :: G.Gr GridPoint a -> G.LEdge b -> GSegment
segment g e = case e of
  (m, n, _) -> (fromJust (G.lab g m), fromJust (G.lab g n))

-- | ポーズ

type GPose = (Maybe [P.BonusUse], GGraph, GGraph)

pposeToGpose :: (P.Figure, P.Pose) -> GPose
pposeToGpose (pfig, ppose) = case ppose of
  P.Pose bonus vs -> (bonus, mkGGraph vs' es, gfig)
    where
      gfig  = pfigToGfig pfig 
      es    = G.labEdges (G.emap (const ()) gfig)
      vs'   = map pointToGridPoint vs

{- |
>>> Just fig = decode "{\"edges\":[[0,1],[0,2],[1,3],[2,3],[2,4],[3,4]],\"vertices\":[[0,20],[20,0],[20,40],[40,20],[49,45]]}" :: Maybe P.Figure
>>> ofig = pfigToGfig fig
>>> gpos = pposeToGpose (fig, P.Pose undefined (map (uncurry P.Point) [(15,0),(35,20),(0,24),(20,44),(30,19)]))
>>> checkPoseByEpsilon 1250 gpos
[(1250,True,(800,800)),(1250,True,(800,801)),(1250,True,(800,801)),(1250,True,(800,800)),(1250,False,(866,925)),(1250,False,(706,725))]
-}
checkPoseByEpsilon :: Int -> GPose -> [(Int, Bool, (GDist, GDist))]
checkPoseByEpsilon ε (_,pos,fig)
  = zipWith check oes pes
    where
      oes = G.labEdges fig
      pes = G.labEdges pos
      check (s,e,seg) (s',e',seg')
        = if | (s,e) /= (s',e')                    -> error "bug!!"
             | 10^(6::Int) * abs (d' - d) <= d * ε -> (ε, True, (d,d'))
             | otherwise                           -> (ε, False, (d,d'))
          where
            d  = dist seg
            d' = dist seg'
{- |
>>> hole = mkHole [(45,80),(35,95),(5,95),(35,50),(5,5),(35,5),(95,95),(65,95),(55,80)]
>>> hole
mkGraph [(0,(45,80)),(1,(35,95)),(2,(5,95)),(3,(35,50)),(4,(5,5)),(5,(35,5)),(6,(95,95)),(7,(65,95)),(8,(55,80))] [(0,1,((45,80),(35,95))),(1,2,((35,95),(5,95))),(2,3,((5,95),(35,50))),(3,4,((35,50),(5,5))),(4,5,((5,5),(35,5))),(5,6,((35,5),(95,95))),(6,7,((95,95),(65,95))),(7,8,((65,95),(55,80))),(8,0,((55,80),(45,80)))]
>>> fig = mkGGraph [(10,93),(85,93),(50,60)] [(0,1,()),(0,2,()),(1,2,())]
>>> fig
mkGraph [(0,(10,93)),(1,(85,93)),(2,(50,60))] [(0,1,((10,93),(85,93))),(0,2,((10,93),(50,60))),(1,2,((85,93),(50,60)))]
>>> isPoseHoleCrossing (undefined,fig,fig) hole
True
-}
isPoseHoleCrossing :: GPose -> GHole -> Bool
isPoseHoleCrossing (_, pose, _) hole 
  = or [ intersect' pseg hseg | pseg <- psegs, hseg <- hsegs ]
    where
      psegs = map (segment pose) (G.labEdges pose)
      hsegs = map (segment hole) (G.labEdges hole)

-- | 頂点の座標
coord :: GVertex -> GridPoint
coord = snd

-- -- | 辺の数
-- size :: G.Gr GridPoint a -> Int
-- size = G.size

-- -- | 頂点の数
-- order :: G.Gr GridPoint a -> Int
-- order = G.order

