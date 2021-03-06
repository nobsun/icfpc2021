{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad
import qualified Data.Aeson                    as JSON
import           Options.Applicative
import           System.Exit                    ( exitFailure )
import           Text.Printf

import qualified PoseInfo


data Options = Options
  { problemNumber :: Int
  , solutionFile  :: FilePath
  }
  deriving stock Show

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optParser) fullDesc

optParser :: Parser Options
optParser =
    Options <$> parseProblemNumber
            <*> parseSolutionFile
  where
    parseProblemNumber = option auto $ mconcat
      [ long "problem"
      , metavar "NUM"
      , help "problem number"
      ]
    parseSolutionFile = strOption $ mconcat
      [ long "solution"
      , help "solution json file"
      , metavar "FILE"
      ]

main :: IO ()
main = do
  Options {..} <- execParser parserInfo
  Just prob <- JSON.decodeFileStrict' (printf "data/problems/%03d.json" problemNumber)
  Just pose <- JSON.decodeFileStrict' solutionFile
  let poseInfo = PoseInfo.verifyPose prob pose
  PoseInfo.reportPoseInfo poseInfo
  unless (PoseInfo.poseIsValid poseInfo) $ do
    exitFailure

printUsageAndExit :: IO ()
printUsageAndExit = do
  putStrLn "Usage: submit --problem NUM --solution FILE"
  exitFailure
