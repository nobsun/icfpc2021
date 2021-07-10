{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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

-- import Data.Aeson
-- import qualified Data.ByteString.Lazy as LBS

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
type GEdge   = G.UEdge

pedgeToGedge :: P.Edge -> GEdge
pedgeToGedge = \ case
  P.Edge s e -> (s,e,())

-- | 穴
type GHole = G.Gr GridPoint ()

-- | 人形
type GFigure = G.Gr GridPoint ()

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
mkFigure vs es = G.mkGraph ns ues
  where
    ns  = zip [0 ..] vs
    ues = map pedgeToGedge es

-- | 辺から線分へ
segment :: G.Gr GridPoint () -> GEdge -> GSegment
segment g e = case e of
  (m, n, _) -> (fromJust (G.lab g m), fromJust (G.lab g n))

-- | 頂点の座標
coord :: GVertex -> GridPoint
coord = snd

-- | 辺の数
size :: G.Gr GridPoint () -> Int
size = G.size

-- | 頂点の数
order :: G.Gr GridPoint () -> Int
order = G.order