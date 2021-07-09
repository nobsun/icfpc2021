-- # 雛形モジュール
-- このファイルは`stack new`コマンドで自動的に`src/`に挿入されます
-- 
-- ## 言語拡張と`module`宣言
-- 最低限の指定をしてある

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( someFunc
    ) where

import Control.Applicative
import Data.Aeson

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

data Figure = Figure { edges :: [Point]
                     , verticecs :: [Point]
                     } deriving Show

instance FromJSON Figure where
  parseJSON = withObject "figure" $ \o -> do
    Figure <$> o .: "edges" <*> o .: "vertices"

instance ToJSON Figure where
  toJSON (Figure es vs)
    = object [ "edges" .= es, "vertices" .= vs]

data Problem = Problem { hole :: [Point]
                       , figure :: Figure
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


-- ## `doctest`のための記述と定義本体
-- テストは失敗するように書いてある

{- | 
「なんか関数」を標準出力に印字する
>>> someFunc
なんか関数
-}
someFunc :: IO ()
someFunc = putStrLn "なんか函数"
