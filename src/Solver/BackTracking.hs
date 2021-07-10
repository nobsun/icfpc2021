module Solver.BackTracking
( Bk(..), mkBk, move, sample1 )
where

import Data.Maybe (listToMaybe)
import Data.List (intersect, sortBy)
import Data.Ord (comparing)

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree as GT
import qualified Data.Map as Map



data Bk = Bk
  { graph :: GT.Gr Int (Int,Int)
  , vertices :: Map.Map Int (Int,Int)
  } deriving Show

mkBk :: [(Int,Int)] -> [(Int,Int)] -> Bk
mkBk vs es =
  Bk
    { graph = G.mkGraph lnodes ledges :: GT.Gr Int (Int,Int)
    , vertices = Map.fromList (zip[0..]vs)
    }
  where
    lnodes = [(i,i) | (i,_)<-zip[0..]vs]
    ledges = [(e,d,(abs(e1-d1),abs(e2-d2))) | (e,d)<-es, let (e1,e2)=vs!!e, let (d1,d2)=vs!!d]


move :: Bk -> Int -> (Int,Int) -> Maybe Bk
move bk p to      =
  listToMaybe $ concat [backtrack bk n newm | n<-G.neighbors g p]
  where
    g = graph bk
    newm = Map.singleton p to

backtrack :: Bk -> Int -> Map.Map Int (Int,Int) -> [Bk]
backtrack bk p newm =
--  traceShow (p,newm) $ 
  concat
  [if done then [Bk{graph=g, vertices=newm'}] else concat[backtrack bk n newm' | n<-G.neighbors g p, Map.notMember n newm']
  |c <-cands
  , let newm'=Map.insert p c newm
  , let done=all (`Map.member`newm') $ G.neighbors g p
  ]
  where
    g = graph bk
    cands :: [(Int,Int)]
    cands = sortp ((vertices bk) Map.! p) $ foldr1 intersect [ nextp (newm Map.! n) (slope g p n)| n<-G.neighbors g p, Map.member n newm]

slope :: GT.Gr Int (Int,Int) -> Int -> Int -> (Int,Int)
slope g p q =
  head [s | (p1,p2,s)<-G.out g p++G.inn g p, p1==q || p2==q]

sortp :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
sortp p xs =
  sortBy (comparing (distance p)) xs

nextp :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
nextp (x,y) (dx,dy) =
  [(x+ a*d, y+ b*e) | a<-[-1,1], b<-[-1,1], (d,e)<-[(dx,dy),(dy,dx)]]

distance :: (Int,Int) -> (Int,Int) -> Int
distance (a,b) (c,d) =
  (a-c)^2 + (b-d)^2


---------------------------------------------------------------------------
sample1 :: Bk
sample1 =
  mkBk vs es
  where
    vs = [(0,0),(10,0),(10,10)]
    es = [(0,1),(1,2),(2,0)]
