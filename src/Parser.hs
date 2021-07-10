{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Parser
    ( Point (..)
    , Index
    , Edge (..)
    , Edges
    , Hole
    , Vertices
    , Figure (..)
    , Problem (..)
    , Pose (..)

    , readProblem
    ) where

-- import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
-- import System.FilePath (FilePath)

-- | data/problems/001.json をパースして Problem にして印字する
--   TODO: サンプルなので API を作ったら破棄して OK です
test001 :: IO ()
test001 = do
  maybeProblem <- readProblem "data/problems/001.json"
  case maybeProblem of
    Just val -> print val
    Nothing  -> putStrLn "fail to decode."

readProblem :: FilePath -> IO (Maybe Problem)
readProblem fp = decode <$> LBS.readFile fp

data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Eq, Ord)
{- |
>>> :set -XOverloadedStrings
>>> decode "[1, 2]" :: Maybe Point
Just (Point {x = 1, y = 2})
-}
instance FromJSON Point where
  parseJSON jsn = do
    [x, y] <- parseJSON jsn
    return $ Point x y

{- |
>>> encode $ Point 1 2
"[1,2]"
-}
instance ToJSON Point where
  toJSON (Point x y) = toJSON [x, y]

type Index = Int

-- | Edge
--
--   NOTE: Index は問題に含まれる vertices の 0 base のインデックスです
data Edge = Edge { s :: Index -- ^ start
                 , e :: Index -- ^ end
                 } deriving (Show, Eq)

{- |
>>> :set -XOverloadedStrings
>>> decode "[1, 2]" :: Maybe Edge
Just (Edge {s = 1, e = 2})
-}
instance FromJSON Edge where
  parseJSON jsn = do
    [x, y] <- parseJSON jsn
    return $ Edge x y

{- |
>>> encode $ Edge 1 2
"[1,2]"
-}
instance ToJSON Edge where
  toJSON (Edge x y) = toJSON [x, y]

type Hole = [Point]
type Vertices = [Point]
type Edges = [Edge]

data Figure = Figure { edges    :: Edges
                     , vertices :: Vertices
                     } deriving (Show, Eq)

{- |
>>> :set -XOverloadedStrings
>>> decode "{\"edges\":[[1,2],[3,4]],\"vertices\":[[5,6]]}" :: Maybe Figure
Just (Figure {edges = [Edge {s = 1, e = 2},Edge {s = 3, e = 4}], vertices = [Point {x = 5, y = 6}]})
-}
instance FromJSON Figure where
  parseJSON = withObject "figure" $ \o -> do
    Figure <$> o .: "edges" <*> o .: "vertices"

{- |
>>> :set -XDuplicateRecordFields
>>> let expected = Figure { edges = [Edge 1 2, Edge 3 4], vertices = [Point 5 6] }
>>> let jsn = encode expected
>>> Just expected == decode jsn
True
-}
instance ToJSON Figure where
  toJSON (Figure es vs)
    = object [ "edges" .= es, "vertices" .= vs]

data Problem = Problem { hole    :: Hole
                       , figure  :: Figure
                       , epsilon :: Int
                       } deriving (Show, Eq)
{- |
>>> decode "{\"epsilon\":1500,\"hole\":[[10,20],[30,40],[50,60]],\"figure\":{\"edges\":[[1,2],[3,4]],\"vertices\":[[5,6]]}}" :: Maybe Problem
Just (Problem {hole = [Point {x = 10, y = 20},Point {x = 30, y = 40},Point {x = 50, y = 60}], figure = Figure {edges = [Edge {s = 1, e = 2},Edge {s = 3, e = 4}], vertices = [Point {x = 5, y = 6}]}, epsilon = 1500})
-}
instance FromJSON Problem where
  parseJSON = withObject "problem" $ \o -> do
    Problem <$> o .: "hole" <*> o .: "figure" <*> o .: "epsilon"

{- |
>>> :set -XDuplicateRecordFields
>>> let h =  [Point 10 20, Point 30 40, Point 50 60]
>>> let f = Figure { edges = [Edge 1 2, Edge 3 4], vertices = [Point 5 6] }
>>> let expected = Problem { hole = h, figure = f, epsilon = 1500 }
>>> let jsn = encode expected
>>> Just expected == decode jsn
True
-}
instance ToJSON Problem where
  toJSON (Problem h f e) = object [ "hole" .= h, "figure" .= f, "epsilon" .= e ]


data Pose = Pose { vertices :: [Point] } deriving (Show, Eq)

{- |
>>> :set -XOverloadedStrings
>>> decode "{\"vertices\":[[1,2],[3,4]]}" :: Maybe Pose
Just (Pose {vertices = [Point {x = 1, y = 2},Point {x = 3, y = 4}]})
-}
instance FromJSON Pose where
  parseJSON = withObject "pose" $ \o -> do
    Pose <$> o .: "vertices"

{- |
>>> encode $ Pose [Point 1 2, Point 3 4]
"{\"vertices\":[[1,2],[3,4]]}"
-}
instance ToJSON Pose where
  toJSON (Pose vs) = object [ "vertices" .= vs ]
