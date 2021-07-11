{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
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
    , BonusDef(..)
    , BonusType(..)
    , BonusUse (..)
    , readProblem
    ) where

import GHC.Generics (Generic)
-- import Control.Applicative
import Data.List (stripPrefix)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
-- import System.FilePath (FilePath)

import AesonArrayGeneric
  (genericParseArrayJSON, defaultAesonArrayOptions, genericToArrayJSON)


readProblem :: FilePath -> IO (Maybe Problem)
readProblem fp = decode <$> LBS.readFile fp


{- $setup
>>> :set -XOverloadedStrings
-}

data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Eq, Ord, Generic)
{- |
>>> decode "[1, 2]" :: Maybe Point
Just (Point {x = 1, y = 2})
-}
instance FromJSON Point where
  parseJSON = genericParseArrayJSON defaultAesonArrayOptions defaultOptions
  {-
  parseJSON jsn = do
    [x, y] <- parseJSON jsn
    return $ Point x y
   -}

{- |
>>> encode $ Point 1 2
"[1,2]"
-}
instance ToJSON Point where
  toJSON = genericToArrayJSON
  -- toJSON (Point x y) = toJSON [x, y]

data BonusType
  = GLOBALIST
  | BREAK_A_LEG deriving (Show, Eq, Generic)

{- |
>>> decode "\"BREAK_A_LEG\"" :: Maybe BonusType
Just BREAK_A_LEG
-}
instance FromJSON BonusType where
  parseJSON = genericParseJSON defaultOptions

{- |
>>> encode BREAK_A_LEG
"\"BREAK_A_LEG\""
-}
instance ToJSON BonusType where
  toJSON = genericToJSON defaultOptions

data BonusDef =
  BonusDef
  { position :: Point
  , bonus :: BonusType
  , problem :: Int
  } deriving (Show, Eq, Generic)

{- |
>>> decode "{\"position\":[1, 2],\"bonus\":\"GLOBALIST\",\"problem\":1}" :: Maybe BonusDef
Just (BonusDef {position = Point {x = 1, y = 2}, bonus = GLOBALIST, problem = 1})
-}
instance FromJSON BonusDef where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON BonusDef where
  toJSON = genericToJSON defaultOptions

type Index = Int

-- | Edge
--
--   NOTE: Index は問題に含まれる vertices の 0 base のインデックスです
data Edge = Edge { s :: Index -- ^ start
                 , e :: Index -- ^ end
                 } deriving (Show, Eq, Generic)

{- |
>>> decode "[1, 2]" :: Maybe Edge
Just (Edge {s = 1, e = 2})
-}
instance FromJSON Edge where
  parseJSON = genericParseArrayJSON defaultAesonArrayOptions defaultOptions
  {-
  parseJSON jsn = do
    [x, y] <- parseJSON jsn
    return $ Edge x y
   -}

{- |
>>> encode $ Edge 1 2
"[1,2]"
-}
instance ToJSON Edge where
  toJSON = genericToArrayJSON
  -- toJSON (Edge x y) = toJSON [x, y]

type Hole = [Point]
type Vertices = [Point]
type Edges = [Edge]

data Figure = Figure { edges    :: Edges
                     , vertices :: Vertices
                     } deriving (Show, Eq, Generic)

{- |
>>> decode "{\"edges\":[[1,2],[3,4]],\"vertices\":[[5,6]]}" :: Maybe Figure
Just (Figure {edges = [Edge {s = 1, e = 2},Edge {s = 3, e = 4}], vertices = [Point {x = 5, y = 6}]})
-}
instance FromJSON Figure where
  parseJSON = genericParseJSON defaultOptions
  {-
  parseJSON = withObject "figure" $ \o -> do
    Figure <$> o .: "edges" <*> o .: "vertices"
   -}

{- |
>>> let expected = Figure { edges = [Edge 1 2, Edge 3 4], vertices = [Point 5 6] }
>>> let jsn = encode expected
>>> Just expected == decode jsn
True
-}
instance ToJSON Figure where
  toJSON = genericToJSON defaultOptions
  {-
  toJSON (Figure es vs)
    = object [ "edges" .= es, "vertices" .= vs]
   -}

data Problem = Problem { bonuses :: Maybe [BonusDef]
                       , hole    :: Hole
                       , figure  :: Figure
                       , epsilon :: Int
                       } deriving (Show, Eq, Generic)
{- |
>>> decode "{\"bonuses\":[{\"bonus\":\"GLOBALIST\",\"problem\":35,\"position\":[62,46]}],\"epsilon\":1500,\"hole\":[[10,20],[30,40],[50,60]],\"figure\":{\"edges\":[[1,2],[3,4]],\"vertices\":[[5,6]]}}" :: Maybe Problem
Just (Problem {bonuses = Just [BonusDef {position = Point {x = 62, y = 46}, bonus = GLOBALIST, problem = 35}], hole = [Point {x = 10, y = 20},Point {x = 30, y = 40},Point {x = 50, y = 60}], figure = Figure {edges = [Edge {s = 1, e = 2},Edge {s = 3, e = 4}], vertices = [Point {x = 5, y = 6}]}, epsilon = 1500})
-}
instance FromJSON Problem where
  parseJSON = genericParseJSON defaultOptions
  {-
  parseJSON = withObject "problem" $ \o -> do
    Problem <$> o .: "hole" <*> o .: "figure" <*> o .: "epsilon"
   -}

{- |
>>> let h =  [Point 10 20, Point 30 40, Point 50 60]
>>> let f = Figure { edges = [Edge 1 2, Edge 3 4], vertices = [Point 5 6] }
>>> let b = Just [BonusDef {position = Point {x = 62, y = 46}, bonus = GLOBALIST, problem = 35}]
>>> let expected = Problem { bonuses = b, hole = h, figure = f, epsilon = 1500 }
>>> let jsn = encode expected
>>> Just expected == decode jsn
True
-}
instance ToJSON Problem where
  toJSON = genericToJSON defaultOptions
  -- toJSON (Problem h f e) = object [ "hole" .= h, "figure" .= f, "epsilon" .= e ]


dropPrefix :: String -> String -> String
dropPrefix prefix x =
  maybe x id $ stripPrefix prefix x

poseOptions :: Options
poseOptions = defaultOptions { fieldLabelModifier = dropPrefix "pose'" }

data BonusUse =
  BonusUse
  { pose'bonus :: BonusType
  , pose'problem :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON BonusUse where
  parseJSON = genericParseJSON poseOptions

instance ToJSON BonusUse where
  toJSON = genericToJSON poseOptions

data Pose = Pose { pose'bonuses :: Maybe [BonusUse], pose'vertices :: [Point] } deriving (Show, Eq, Generic)

{- |
>>> decode "{\"bonuses\":[{\"bonus\":\"GLOBALIST\",\"problem\":35}],\"vertices\":[[1,2],[3,4]]}" :: Maybe Pose
Just (Pose {pose'bonuses = Just [BonusUse {pose'bonus = GLOBALIST, pose'problem = 35}], pose'vertices = [Point {x = 1, y = 2},Point {x = 3, y = 4}]})
-}
instance FromJSON Pose where
  parseJSON = genericParseJSON poseOptions
  {-
  parseJSON = withObject "pose" $ \o -> do
    Pose <$> o .: "vertices"
   -}

{- |
>>> let expected = Pose (Just [BonusUse GLOBALIST 35]) [Point 1 2, Point 3 4]
>>> let jsn = encode expected
>>> Just expected == decode jsn
True
-}
instance ToJSON Pose where
  toJSON = genericToJSON poseOptions
  -- toJSON (Pose vs) = object [ "vertices" .= vs ]
