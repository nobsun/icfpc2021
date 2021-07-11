
module SubmitQueue (
  submitGateway,
  ) where

import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import System.IO.Error (tryIOError)
import Data.Char (isDigit)
import Data.List (isSuffixOf)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Time
  (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime, getCurrentTime,
   ZonedTime, getZonedTime, utcToLocalZonedTime, zonedTimeToUTC,
   FormatTime, formatTime, parseTimeM, defaultTimeLocale)
import Text.Printf (printf)
import System.IO
  (stdout, BufferMode (LineBuffering), hSetBuffering, hGetContents,
   IOMode (ReadMode), withFile, hGetLine)
import System.FilePath ((</>), (<.>), dropExtension)
import System.Directory (doesFileExist, renameFile)
import System.Process (rawSystem, runInteractiveProcess)

import ProcessIO (ioExitCode)


submitGateway :: IO ()
submitGateway = do
  hSetBuffering stdout LineBuffering
  putLog <- do
    put <- newLog
    return $ \s -> do
      ts <- formatLogStamp <$> getZonedTime
      put $ ts ++ ": " ++ s
  waitRequest putLog =<< newIORef Map.empty

newLog :: IO (String -> IO ())
newLog = do
  c <- newChan
  void . forkIO . forever $ putStrLn =<< readChan c
  return $ writeChan c

-----

type QueueMap = Map Int (Chan FilePath)

--  post --inotify--> queue
waitRequest :: (String -> IO ()) -> IORef QueueMap -> IO ()
waitRequest putLog_ qmRef = do
  let putLogLn = putLog_ . ("request: " ++)
      newQueue problemId = do
        q <- newChan
        putLogLn $ "newqueue: starting new thead for problem " ++ show problemId
        void . forkIO $ submitLoop putLog_ problemId (submit putLog_ problemId q)
        modifyIORef' qmRef $ Map.insert problemId q
        return q

      enqueue problemId fn = do
        ts <- formatFileStamp <$> getZonedTime
        let qfn = printf "%03d" problemId ++ "-" ++ ts <.> "json"
        putLogLn $ "enqueue: " ++ fn ++ " --> " ++ qfn
        renameFile (postDir </> fn) (queueDir </> qfn)

        m <- readIORef qmRef
        q <- maybe (newQueue problemId) return $ Map.lookup problemId m
        writeChan q qfn

      process req = case req of
        [_d, _ev, fn]
          | ".json" `isSuffixOf` fn
          , let name = dropExtension fn
          , all isDigit name
          , (problemId, ""):_ <- reads name  -> enqueue problemId fn
        [_d, _ev, fn]                        -> putLogLn $ "unknown filename pattern. ignored: " ++ fn
        _                                    -> return ()

  reqs <- readInotifyEvents
  putLog_ "started. waiting for requests..."
  mapM_ process reqs

-- returns list of [directory, events, filename]
readInotifyEvents :: IO [[String]]
readInotifyEvents = do
  (_in', out, _err, _ph) <- runInteractiveProcess
                            "/usr/bin/inotifywait" ["-m", "-e", "close_write", postDir]
                            Nothing Nothing
  hSetBuffering out LineBuffering
  map words . lines <$> hGetContents out

-----

submitLoop :: (String -> IO ()) -> Int -> IO () -> IO ()
submitLoop putLog_ problemId submit_ =
    loop =<< loadStamp putLog_ name
  where
    name = printf "%03d" problemId
    putLog = putLog_ . (printf "submit: problem %3d: " problemId ++)
    loop maySubmitTs = do
      let delayRest prev = do
            current <- getCurrentTime
            let waitt = interval - diffUTCTime current prev
            prevL <- utcToLocalZonedTime prev
            nextL <- utcToLocalZonedTime $ addUTCTime waitt current
            putLog $ unwords
              [ "previouns post at",
                formatLogStamp prevL ++ ".",
                "so, wait", show waitt ++ ".",
                "will proceed at",
                formatLogStamp nextL ++ "." ]
            threadDelay $ fromEnum waitt `quot` 1000000
      maybe (return ()) delayRest maySubmitTs
      putLog "now, dequeue next submit."
      putLog . either (\e -> "submit failed: " ++ show e) (\() -> "submit done.")
        =<< tryIOError submit_
      loop . Just =<< saveStamp putLog_ name

interval :: NominalDiffTime
interval = fromInteger $ 5 * 60

submit :: (String -> IO ()) -> Int -> Chan (FilePath) -> IO ()
submit putLog_ problemId q = do
  fn <- readChan q
  putLog_ $ "submit: " ++ (queueDir </> fn) ++ " --> " ++ (submitDir </> fn)
  renameFile (queueDir </> fn) (submitDir </> fn)
  ioExitCode =<< rawSystem "./lib/do-submit.sh" [show problemId, submitDir </> fn]

formatFileStamp :: FormatTime t => t -> String
formatFileStamp = formatTime defaultTimeLocale "%d-%H%M%S"

-----

formatLogStamp :: FormatTime t => t -> String
formatLogStamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

---

loadStamp :: (String -> IO ()) -> String -> IO (Maybe UTCTime)
loadStamp putLog_ name = do
  exist <- checkStamp_ (stampFn name)
  if exist
    then do zt <- loadStamp_ (stampFn name)
            putLog_ $ "stamp: loaded timestamp for name: " ++ name ++ ": " ++ show zt
            return $ Just $ zonedTimeToUTC zt
    else    return Nothing

loadStamp_ :: String -> IO ZonedTime
loadStamp_ fn = do
  s <- withFile (stateDir </> fn) ReadMode hGetLine
  let failed = fail $ "stamp: fail to load timestamp from file: " ++ fn
  maybe failed return $ parseTimeM False defaultTimeLocale stampFormat s

checkStamp_ :: FilePath -> IO Bool
checkStamp_ = doesFileExist . (stateDir </>)

saveStamp :: (String -> IO ()) -> String -> IO UTCTime
saveStamp putLog_ name = do
  putLog_ $ "stamp: saving timestamp for name: " ++ name
  (zonedTimeToUTC <$>) . saveStamp_ $ stampFn name

saveStamp_ :: FilePath -> IO ZonedTime
saveStamp_ fn = do
  ct <- getZonedTime
  let stamp = formatTime defaultTimeLocale stampFormat ct
  writeFile (stateDir </> fn) $ unlines [stamp]
  return ct

stampFn :: String -> FilePath
stampFn = (<.> "ts")

stampFormat :: String
stampFormat = "%Y-%m-%d %H:%M:%S %z"

-----

-- state dir to save timestamp

stateDir :: FilePath
stateDir = "/home/icfpc2021/submit/state"

-- post --> queue --> submit

postDir :: FilePath
postDir = "/home/icfpc2021/submit/post"

queueDir :: FilePath
queueDir = "/home/icfpc2021/submit/queue"

submitDir :: FilePath
submitDir = "/home/icfpc2021/submit/submit"
