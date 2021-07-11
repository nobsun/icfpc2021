{-# LANGUAGE NamedFieldPuns #-}

module Solver.BackTracking
( solve, Bk(..), mkBk, move, autoTuneEdges, sample1, sample2, sample3 )
where

import Data.Maybe (listToMaybe, maybeToList)
import Data.List (intersect, sortBy)
import Data.Ord (comparing)

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree as GT
import qualified Data.Map as Map

import Types
import qualified Parser as P
--import qualified Segment as S (intersect)
import qualified Score as Score (tolerant)

--import Debug.Trace


-- note
-- GridPoint = (Int, Int)
-- GSegment = (GridPoint, GridPoint)

type GSlope = (Int,Int)


solve :: P.Problem -> IO P.Pose
solve prob =
  return P.Pose{P.pose'bonuses=Nothing, P.pose'vertices=vs'}
  where
    P.Figure{ P.edges = _es', P.vertices = vs'} = P.figure prob

p2gp :: P.Point -> GridPoint
p2gp P.Point{P.x=x, P.y=y} = (x, y)

e2gp :: P.Edge -> GridPoint
e2gp P.Edge{P.s=s, P.e=e} = (s, e)



data Bk = Bk
  { graph    :: GT.Gr Int GSlope
  , vertices :: Map.Map Int GridPoint
  , hole     :: [GridPoint]
  , epsilon  :: Int
  } deriving Show

mkBk :: P.Problem -> Bk
mkBk prob=
  mkBk' vs es (map p2gp (P.hole prob)) (P.epsilon prob)
  where
    P.Figure{ P.edges = es', P.vertices = vs'} = P.figure prob
    es = map e2gp es'
    vs = map p2gp vs'

mkBk' :: [GridPoint] -> [GridPoint] -> [GridPoint] -> Int -> Bk
mkBk' vs es hole eps =
  Bk{ graph = G.mkGraph lnodes ledges :: GT.Gr Int GSlope
    , vertices = Map.fromList (zip [0..] vs)
    , hole = hole
    , epsilon = eps
    }
  where
    lnodes = [(i,i) | (i,_)<-zip[0..]vs]
    -- 辺のラベルに傾き(Int,Int)を入れておく. この傾き方向を探索する
    ledges = [(e,d,(abs(e1-d1),abs(e2-d2))) | (e,d)<-es, let (e1,e2)=vs!!e, let (d1,d2)=vs!!d]

move :: Bk
     -> Int       -- ^ 疑似ランダム用
     -> Int       -- ^ 動かす頂点の番号
     -> GridPoint -- ^ 動かす先の座標
     -> Maybe Bk  -- ^ 失敗したらNothing
move bk@Bk{graph=g} rand p to      =
  listToMaybe $ concat [backtrack bk depth rand n newm | n<-G.neighbors g p]
  where
    newm = Map.singleton p to
    depth = length (G.nodes g) + length (G.edges g)


backtrack :: Bk
          -> Int                     -- ^ 探索の深さ制限
          -> Int                     -- ^ 疑似ランダム用
          -> Int                     -- ^ 今から動かす点. 前提としてnewmのいづれかの点と辺があること.
          -> Map.Map Int GridPoint   -- ^ 動かし済みの点の集合
          -> [Bk]                    -- ^ 見つかれば返す. 見つからなかったら空
backtrack _bk 0 _rand _p _newm = []
backtrack bk@Bk{graph=g, vertices=vmap, epsilon=eps} depth rand p newm  =
--  traceShow (p,newm) $
  concat
  [if done then [bk{vertices=newm'}]
           else [bk{vertices=newm''} | newm'' <-maybeToList $ foldr (\n m-> m>>=merge n) (Just newm') (G.neighbors g p)]
  |c <-cands
  , let newm'=Map.insert p c newm
  , let done=all (`Map.member`newm') $ G.neighbors g p
  ]
  where
    -- まず点pの行き先候補を探す
    -- 動かし済みの点集合でpに隣接する点たちから, 全部の傾きを考慮して共通点を取るとpの行き先候補になる
    cands :: [GridPoint]
    cands = sortp rand (vmap Map.! p)
          $ foldr1 intersect
            [ nextp (newm Map.! n) (slope g p n) eps | n<-ns]

    -- 点pの隣接点ですでに動かし済みのもの
    ns :: [Int]
    ns = [ n | n<-G.neighbors g p, Map.member n newm]

    merge :: Int -> Map.Map Int GSlope -> Maybe (Map.Map Int GSlope)
    merge n m | Map.member n m = Just m
              | otherwise =
      case backtrack bk (depth-1) rand n m of
        []      -> Nothing
        (bk':_) -> Just (Map.union m (vertices bk'))

-- 頂点 p, q の間の辺の傾きを取得する.
-- p, q の間には必ず辺があることが前提.
slope :: GT.Gr Int GSlope -> Int -> Int -> GSlope
slope g p q =
  head [s | (p1,p2,s)<-G.out g p++G.inn g p, p1==q || p2==q]

-- 点p に近い順に並べる
sortp :: Int -> GridPoint -> [GridPoint] -> [GridPoint]
sortp rand p xs =
  map snd $
  case ys of
    (a@(da,_):b@(db,_):cs) | da==db && rand`mod`2==0 -> b:a:cs
    _ -> ys
  where
    ys = sortBy (comparing fst) [(distance p x,x) | x<-xs]

{-
-- 候補の座標リストからホールに入らないものを除外する
filterp :: Bk
        -> [GridPoint] -- 点pの隣接点
        -> [GridPoint] -- 候補点
        -> [GridPoint]
filterp Bk{hole=hole} ps qs =
  [ q | p<-ps,  q<-qs, not(or[S.intersect (p,q) (h1,h2) | (h1,h2)<-hs])]
  where
    -- ホールの全部の辺
    hs = zip hole (tail hole++hole)
-}

-- 傾きとEpsilonから隣接点の候補リストを取得する
nextp :: GridPoint -> GSlope -> Int -> [GridPoint]
nextp (x,y) (dx,dy) _eps =
  [ (x+ a*d, y+ b*e)
  | a<-[-1,1]
  , b<-[-1,1]
  , (d,e)<-[(dx,dy),(dy,dx)]
  ]

distance :: GridPoint -> GridPoint -> Int
distance (a,b) (c,d) =
  (a-c)^(2::Int) + (b-d)^(2::Int)

autoTuneEdges :: Bk -> Bk
autoTuneEdges =
  go 10000
  where
    go 0 bk = bk
    go n bk = case autoTuneEdge1 bk of
                 Nothing -> bk
                 Just bk'-> go (n-1) bk'

autoTuneEdge1 :: Bk -> Maybe Bk
autoTuneEdge1 bk@Bk{graph=g, vertices=vmap, epsilon=eps} =
  case take 1 tune of
    []         -> Nothing
    [(p, pos)] -> Just bk{vertices=Map.insert p pos vmap}
  where
    invalidNodes = [(p,q,spq) | (p,q,spq) <-G.labEdges g, not $ Score.tolerant eps ((0,0),spq) (vmap Map.! p, vmap Map.! q)]
    adjs = [(dx,dy) | d<-[1,2..], dx<-[-d,-d+1..d], dy<-[-d,-d+1..d], (abs dx)+(abs dy)>=d]

    tune :: [(Int,GridPoint)]
    tune = [ if pok then (p, (p1+dx,p2+dy)) else (q, (q1+dx,q2+dy))
           | (p,q,spq) <- invalidNodes
           , let (p1,p2) = vmap Map.! p
           , let (q1,q2) = vmap Map.! q
           , (dx,dy)<-take 400 adjs
           , let pok = Score.tolerant eps ((0,0),spq) ((p1+dx,p2+dy),(q1,q2))
           , let qok = Score.tolerant eps ((0,0),spq) ((p1,p2),(q1+dx,q2+dy))
           , pok || qok
           ]





---------------------------------------------------------------------------
-- triangle (problem 11)
sample1 :: Bk
sample1 =
  mkBk' vs es vs 0
  where
    vs = [(0,0),(10,0),(10,10)]
    es = [(0,1),(1,2),(2,0)]

-- line
sample3 :: Bk
sample3 =
  mkBk' vs es vs 0
  where
    vs = [(0,0),(10,0),(10,10)]
    es = [(0,1),(2,0)]

-- stcikman (problem 1)
sample2 :: Bk
sample2 =
  mkBk' vs es vs 0
  where
    vs = [(20, 30), (20, 40), (30, 95), (40, 15), (40, 35), (40, 65),(40, 95), (45, 5), (45, 25), (50, 15), (50, 70), (55, 5),(55, 25), (60, 15), (60, 35), (60, 65), (60, 95), (70, 95),(80, 30), (80, 40)]
    es = [(2, 5), (5, 4), (4, 1), (1, 0), (0, 8), (8, 3), (3, 7),(7, 11), (11, 13), (13, 12), (12, 18), (18, 19), (19, 14),(14, 15), (15, 17), (17, 16), (16, 10), (10, 6), (6, 2),(8, 12), (7, 9), (9, 3), (8, 9), (9, 12), (13, 9), (9, 11),(4, 8), (12, 14), (5, 10), (10, 15)]
