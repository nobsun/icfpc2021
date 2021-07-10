module Solver.BackTracking
( Bk(..), mkBk, move, sample1, sample2 )
where

import Data.Maybe (listToMaybe, maybeToList)
import Data.List (intersect, sortBy)
import Data.Ord (comparing)

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree as GT
import qualified Data.Map as Map


--import Debug.Trace

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
  [if done then [bk{vertices=newm'}]
           else [bk{vertices=newm''} | newm'' <-maybeToList $ foldr (\n m-> m>>=merge n) (Just newm') (G.neighbors g p)]
  |c <-cands
  , let newm'=Map.insert p c newm
  , let done=all (`Map.member`newm') $ G.neighbors g p
  ]
  where
    g = graph bk

    cands :: [(Int,Int)]
    cands = sortp ((vertices bk) Map.! p) $ foldr1 intersect [ nextp (newm Map.! n) (slope g p n)| n<-G.neighbors g p, Map.member n newm]

    merge :: Int -> Map.Map Int (Int,Int) -> Maybe (Map.Map Int (Int,Int))
    merge n m | Map.member n m = Just m
              | otherwise =
      case backtrack bk n m of
        []      -> Nothing
        (bk':_) -> Just (Map.union m (vertices bk'))

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
  (a-c)^(2::Int) + (b-d)^(2::Int)


---------------------------------------------------------------------------
-- triangle (problem 11)
sample1 :: Bk
sample1 =
  mkBk vs es
  where
    vs = [(0,0),(10,0),(10,10)]
    es = [(0,1),(1,2),(2,0)]

-- stcikman (problem 1)
sample2 :: Bk
sample2 =
  mkBk vs es
  where
    vs = [(20, 30), (20, 40), (30, 95), (40, 15), (40, 35), (40, 65),(40, 95), (45, 5), (45, 25), (50, 15), (50, 70), (55, 5),(55, 25), (60, 15), (60, 35), (60, 65), (60, 95), (70, 95),(80, 30), (80, 40)]
    es = [(2, 5), (5, 4), (4, 1), (1, 0), (0, 8), (8, 3), (3, 7),(7, 11), (11, 13), (13, 12), (12, 18), (18, 19), (19, 14),(14, 15), (15, 17), (17, 16), (16, 10), (10, 6), (6, 2),(8, 12), (7, 9), (9, 3), (8, 9), (9, 12), (13, 9), (9, 11),(4, 8), (12, 14), (5, 10), (10, 15)]
