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
>>> checkPoseByEpsilon 1250 (undefined,[(15,0),(35,20),(0,24),(20,44),(30,19)],ofig)
[(1250,True,(800,800)),(1250,True,(800,801)),(1250,True,(800,801)),(1250,True,(800,800)),(1250,False,(866,925)),(1250,False,(706,725))]
>>> checkPoseByEpsilon 1250 (undefined,[(15,0),(35,20),(0,24),(20,44),(29,19)],ofig)
[(1250,True,(800,800)),(1250,True,(800,801)),(1250,True,(800,801)),(1250,True,(800,800)),(1250,True,(866,866)),(1250,True,(706,706))]
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

-- | 頂点の座標
coord :: GVertex -> GridPoint
coord = snd

-- -- | 辺の数
-- size :: G.Gr GridPoint a -> Int
-- size = G.size

-- -- | 頂点の数
-- order :: G.Gr GridPoint a -> Int
-- order = G.order

