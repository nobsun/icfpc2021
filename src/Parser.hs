{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser
    ( Point (..)
    , Index
    , Edge (..)
    , Edges
    , Hole
    , Vertices
    , Figure (..)
    , Problem (..)
    , Pose
    
    , readProblem
    ) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import System.FilePath (FilePath)

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
                   } deriving (Show, Eq)

instance FromJSON Point where
  parseJSON jsn = do
    [x, y] <- parseJSON jsn
    return $ Point x y

instance ToJSON Point where
  toJSON (Point x y) = toJSON [x, y]

type Index = Int

-- | Edge
--
--   NOTE: Index は問題に含まれる vertices の 0 base のインデックスです
data Edge = Edge { s :: Index -- ^ start
                 , e :: Index -- ^ end
                 } deriving (Show, Eq)

instance FromJSON Edge where
  parseJSON jsn = do
    [x, y] <- parseJSON jsn
    return $ Edge x y

instance ToJSON Edge where
  toJSON (Edge x y) = toJSON [x, y]

type Hole = [Point]
type Vertices = [Point]
type Edges = [Edge]

data Figure = Figure { edges    :: Edges
                     , vertices :: Vertices
                     } deriving Show

instance FromJSON Figure where
  parseJSON = withObject "figure" $ \o -> do
    Figure <$> o .: "edges" <*> o .: "vertices"

instance ToJSON Figure where
  toJSON (Figure es vs)
    = object [ "edges" .= es, "vertices" .= vs]

data Problem = Problem { hole    :: Hole
                       , figure  :: Figure
                       , epsilon :: Int
                       } deriving Show

instance FromJSON Problem where
  parseJSON = withObject "problem" $ \o -> do
    Problem <$> o .: "hole" <*> o .: "figure" <*> o .: "epsilon"

instance ToJSON Problem where
  toJSON (Problem h f e) = object [ "hole" .= h, "figure" .= f, "epsilon" .= e ]


data Pose = Pose { vertices :: [Point] } deriving Show

instance FromJSON Pose where
  parseJSON = withObject "pose" $ \o -> do
    Pose <$> o .: "vertices"

instance ToJSON Pose where
  toJSON (Pose vs) = object [ "vertices" .= vs ]

