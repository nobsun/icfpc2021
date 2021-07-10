{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Functor                   ( (<&>) )
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Network.HTTP.Client            ( Request(..) )
import qualified Network.HTTP.Simple           as HTTP
import           Options.Applicative
import           System.Environment             ( lookupEnv )
import           System.Exit                    ( exitFailure )

data Options = Options
  { problemNumber :: Maybe Int
  , solutionFile  :: Maybe FilePath
  , testHello     :: Bool
  }
  deriving stock Show

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optParser) fullDesc

optParser :: Parser Options
optParser =
    Options <$> parseProblemNumber
            <*> parseSolutionFile
            <*> parseTestHello
  where
    parseProblemNumber = optional $ option auto $ mconcat
      [ long "problem"
      , metavar "NUM"
      , help "problem number"
      ]
    parseSolutionFile = optional $ strOption $ mconcat
      [ long "solution"
      , help "solution json file"
      , metavar "FILE"
      ]
    parseTestHello = switch $ mconcat
      [ long "test-hello"
      , help "test `GET api/hello` (no submit)"
      ]

main :: IO ()
main = do
  Options {..} <- execParser parserInfo
  apiToken <- getApiToken
  if testHello
    then testHelloApi apiToken
    else case (problemNumber, solutionFile) of
      (Just n, Just s) -> submit apiToken n s
      _ -> printUsageAndExit


testHelloApi :: B.ByteString -> IO ()
testHelloApi apiToken = do
  req <- HTTP.parseRequest "GET https://poses.live/api/hello"
     <&> HTTP.setRequestHeader "Authorization" ["Bearer " <> apiToken]
  sendRequest req

submit :: B.ByteString -> Int -> FilePath -> IO ()
submit apiToken problemNumber solutionFile = do
  let url = "POST https://poses.live/api/problems/" <> show problemNumber <> "/solutions"
  req <- HTTP.parseRequest url
     <&> HTTP.setRequestHeader "Authorization" ["Bearer " <> apiToken]
     <&> HTTP.setRequestBodyFile solutionFile
  sendRequest req

sendRequest :: Request -> IO ()
sendRequest req = do
  resp <- HTTP.httpBS req
  if HTTP.getResponseStatusCode resp == 200
    then BC.putStrLn $ HTTP.getResponseBody resp
    else do
      print resp
      exitFailure

getApiToken :: IO B.ByteString
getApiToken = lookupEnv _API_TOKEN_ENV >>= \case
  Nothing -> error $ "environment variable " <> _API_TOKEN_ENV <> " not set"
  Just apiToken -> pure (fromString apiToken)

_API_TOKEN_ENV :: String
_API_TOKEN_ENV = "ICFP2021_API_TOKEN"

printUsageAndExit :: IO ()
printUsageAndExit = do
  putStrLn "Usage: submit --problem NUM --solution FILE"
  exitFailure

