{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Main
  ( main
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent.STM         ( TQueue
                                                , atomically
                                                , newTQueueIO
                                                , tryReadTQueue
                                                , writeTQueue
                                                )
import           Control.Monad                  ( forM_
                                                , void
                                                , when
                                                )
import           Control.Monad.RWS.Strict       ( RWST
                                                , ask
                                                , asks
                                                , evalRWST
                                                , get
                                                , gets
                                                , liftIO
                                                , modify
                                                )
import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy.Char8    as BLC
import           Data.Function                  ( on )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.StateVar                  ( ($=) )
import           Options.Applicative
import           System.IO                      ( BufferMode(LineBuffering)
                                                , hSetBuffering
                                                , stderr
                                                , stdout
                                                )
import           Text.Printf                    ( printf )

import qualified Graphics.Rendering.OpenGL     as GL
import qualified Graphics.Rendering.OpenGL.GL.LineSegments
                                               as GL.LineSegments
import qualified Graphics.Rendering.OpenGL.GL.Points
                                               as GL.Points
import qualified Graphics.UI.GLFW              as GLFW

import           Data.List                      ( minimumBy )
import           GHC.IO                         ( evaluate )
import qualified Hole
import qualified Parser                        as P
import           Parser                         ( Figure(..) )
import           PoseInfo                       ( PoseEdgeInfo(..)
                                                , PoseInfo(..)
                                                , PoseVertexInfo(..)
                                                )
import qualified PoseInfo
import qualified Solver.BackTracking           as Bk
import qualified TwoDim

--------------------------------------------------------- -----------------------

data Env = Env
  { envEventsChan  :: TQueue Event
  , envWindow      :: !GLFW.Window
  , envOptions     :: Options
  , envProblem     :: P.Problem
  , envCanvasSizeX :: Int
  , envCanvasSizeY :: Int
  }

data State = State
  { stateWindowWidth      :: !Int
  , stateWindowHeight     :: !Int
  , stateFBWidth          :: !Int
  , stateFBHeight         :: !Int
  , statePoint            :: Maybe (Int, Int)
  , statePose             :: PoseInfo
  , stateDislike          :: !Int
  , stateSelectedVertexId :: Maybe Int
  , stateSelectedEdgeId   :: Maybe Int
  , stateUndoQueue        :: [P.Pose]
  , stateRedoQueue        :: [P.Pose]
  }

type Demo = RWST Env () State IO

_Bairitsu :: Integral a => a
_Bairitsu = 1

--------------------------------------------------------------------------------

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !Bool
  | EventWindowIconify   !GLFW.Window !Bool
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

data Options = Options
  { optHistory       :: Maybe [(Int, Int)]
  , optDumpEvents    :: Bool
  , optProblemNumber :: Int
  , optPose          :: Maybe FilePath
  , optPrintGrid     :: Bool
  }
  deriving Show

optionsParser :: Parser Options
optionsParser =
  Options <$> history <*> dumpEvents <*> problemNumber <*> pose <*> printGrid
 where
  history :: Parser (Maybe [(Int, Int)])
  history = optional $ option auto $ mconcat
    [long "history", metavar "STR", help "history (type: [(Int,Int)])"]

  dumpEvents :: Parser Bool
  dumpEvents = switch $ mconcat [long "dump-events", help "dump OpenGL events"]

  printGrid :: Parser Bool
  printGrid = switch $ mconcat [long "print-grid", help "print grid"]

  problemNumber :: Parser Int
  problemNumber = option auto
    $ mconcat [long "problem", help "problem number", metavar "NUM", value 1]

  pose :: Parser (Maybe FilePath)
  pose = optional $ strOption $ mconcat [long "pose", metavar "FILE"]

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) fullDesc

main :: IO ()
main = do
  opt     <- execParser parserInfo

  problem <- fromMaybe (error "parsing problem") <$> P.readProblem
    (printf "./data/problems/%03d.json" (optProblemNumber opt))
  mPose <- case optPose opt of
    Nothing -> pure Nothing
    Just f  -> fromMaybe (error "parsing pose") <$> A.decodeFileStrict' f

  let
    width  = 1920
    height = 1080
    problemSize =
      maximum (concat [ [x, y] | P.Point x y <- P.hole problem ]) `div` 10 * 15

  eventsChan <- newTQueueIO :: IO (TQueue Event)

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  withWindow width height "ICFPc 2021" $ \win -> do
    GLFW.setErrorCallback $ Just $ errorCallback eventsChan
    GLFW.setWindowPosCallback win $ Just $ windowPosCallback eventsChan
    GLFW.setWindowSizeCallback win $ Just $ windowSizeCallback eventsChan
    GLFW.setWindowCloseCallback win $ Just $ windowCloseCallback eventsChan
    GLFW.setWindowRefreshCallback win $ Just $ windowRefreshCallback eventsChan
    GLFW.setWindowFocusCallback win $ Just $ windowFocusCallback eventsChan
    GLFW.setWindowIconifyCallback win $ Just $ windowIconifyCallback eventsChan
    GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback
      eventsChan
    GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback eventsChan
    GLFW.setCursorPosCallback win $ Just $ cursorPosCallback eventsChan
    GLFW.setCursorEnterCallback win $ Just $ cursorEnterCallback eventsChan
    GLFW.setScrollCallback win $ Just $ scrollCallback eventsChan
    GLFW.setKeyCallback win $ Just $ keyCallback eventsChan
    GLFW.setCharCallback win $ Just $ charCallback eventsChan

    GLFW.swapInterval 1

    (fbWidth , fbHeight ) <- GLFW.getFramebufferSize win
    (winWidth, winHeight) <- GLFW.getWindowSize win

    let env = Env { envEventsChan  = eventsChan
                  , envWindow      = win
                  , envOptions     = opt
                  , envProblem     = problem
                  , envCanvasSizeX = problemSize * _Bairitsu
                  , envCanvasSizeY = problemSize * _Bairitsu
                  }
        pose = case mPose of
          Nothing    -> P.Pose Nothing $ vertices (P.figure problem)
          Just pose' -> pose'
        state_ = State { stateWindowWidth      = winWidth
                       , stateWindowHeight     = winHeight
                       , stateFBWidth          = fbWidth
                       , stateFBHeight         = fbHeight
                       , statePoint            = Nothing
                       , statePose = PoseInfo.verifyPose problem pose
                       , stateDislike          = 0
                       , stateSelectedVertexId = Nothing
                       , stateSelectedEdgeId   = Nothing
                       , stateUndoQueue        = []
                       , stateRedoQueue        = []
                       }

    runDemo env state_

  putStrLn "ended!"

--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
  where simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback :: TQueue Event -> GLFW.Error -> String -> IO ()
windowPosCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowCloseCallback :: TQueue Event -> GLFW.Window -> IO ()
windowRefreshCallback :: TQueue Event -> GLFW.Window -> IO ()
windowFocusCallback :: TQueue Event -> GLFW.Window -> Bool -> IO ()
windowIconifyCallback :: TQueue Event -> GLFW.Window -> Bool -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
mouseButtonCallback
  :: TQueue Event
  -> GLFW.Window
  -> GLFW.MouseButton
  -> GLFW.MouseButtonState
  -> GLFW.ModifierKeys
  -> IO ()
cursorPosCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
cursorEnterCallback :: TQueue Event -> GLFW.Window -> GLFW.CursorState -> IO ()
scrollCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
keyCallback
  :: TQueue Event
  -> GLFW.Window
  -> GLFW.Key
  -> Int
  -> GLFW.KeyState
  -> GLFW.ModifierKeys
  -> IO ()
charCallback :: TQueue Event -> GLFW.Window -> Char -> IO ()

errorCallback tc e s = atomically $ writeTQueue tc $ EventError e s
windowPosCallback tc win x y =
  atomically $ writeTQueue tc $ EventWindowPos win x y
windowSizeCallback tc win w h =
  atomically $ writeTQueue tc $ EventWindowSize win w h
windowCloseCallback tc win = atomically $ writeTQueue tc $ EventWindowClose win
windowRefreshCallback tc win =
  atomically $ writeTQueue tc $ EventWindowRefresh win
windowFocusCallback tc win fa =
  atomically $ writeTQueue tc $ EventWindowFocus win fa
windowIconifyCallback tc win ia =
  atomically $ writeTQueue tc $ EventWindowIconify win ia
framebufferSizeCallback tc win w h =
  atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback tc win mb mba mk =
  atomically $ writeTQueue tc $ EventMouseButton win mb mba mk
cursorPosCallback tc win x y =
  atomically $ writeTQueue tc $ EventCursorPos win x y
cursorEnterCallback tc win ca =
  atomically $ writeTQueue tc $ EventCursorEnter win ca
scrollCallback tc win x y = atomically $ writeTQueue tc $ EventScroll win x y
keyCallback tc win k sc ka mk =
  atomically $ writeTQueue tc $ EventKey win k sc ka mk
charCallback tc win c = atomically $ writeTQueue tc $ EventChar win c

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state = void $ evalRWST (adjustWindow >> run) env state

run :: Demo ()
run = do
  liftIO GLFW.waitEvents
  processEvents

  win <- asks envWindow
  q   <- liftIO $ GLFW.windowShouldClose win
  if q
    then do
      pure ()
    else do
      run

processEvents :: Demo ()
processEvents = do
  tc <- asks envEventsChan
  me <- liftIO $ atomically $ tryReadTQueue tc
  case me of
    Just e -> do
      processEvent e
      processEvents
    Nothing -> return ()

processEvent :: Event -> Demo ()
processEvent ev = case ev of
  (EventError e s) -> do
    printEvent "error" [show e, show s]
    win <- asks envWindow
    liftIO $ GLFW.setWindowShouldClose win True

  (EventWindowPos  _ x     y     ) -> printEvent "window pos" [show x, show y]

  (EventWindowSize _ width height) -> do
    printEvent "window size" [show width, show height]
    modify $ \s -> s { stateWindowWidth = width, stateWindowHeight = height }
    adjustWindow

  (EventWindowClose   _               ) -> printEvent "window close" []

  (EventWindowRefresh _               ) -> printEvent "window refresh" []

  (EventWindowFocus   _ fs            ) -> printEvent "window focus" [show fs]

  (EventWindowIconify _ is            ) -> printEvent "window iconify" [show is]

  (EventFramebufferSize _ width height) -> do
    printEvent "framebuffer size" [show width, show height]
    modify $ \s -> s { stateFBWidth = width, stateFBHeight = height }
    adjustWindow

  (EventMouseButton _ mb mbs mk) -> do
    printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
    poseInfo@PoseInfo {..} <- gets statePose
    (x, y)                 <- getCursorPos
    when (mb == GLFW.MouseButton'1 && mbs == GLFW.MouseButtonState'Pressed) $ do
      gets stateSelectedVertexId >>= \case
        Nothing -> do
          let v = nearestVertex (x, y) poseVertexInfo
          modify $ \s -> s { stateSelectedVertexId = Just (vertexId v) }
        Just _ -> do
          pure ()
    when (mb == GLFW.MouseButton'1 && mbs == GLFW.MouseButtonState'Released)
      $ do
          gets stateSelectedVertexId >>= \case
            Nothing -> do
              pure ()
            Just vnum -> do
              let currentPose = poseOfPoseInfo poseInfo
                  newPose     = updatePosOf vnum (P.Point x y) currentPose
              problem <- asks envProblem
              liftIO $ putStrLn $ "selected: " <> show vnum
              modify $ \s -> s { stateSelectedVertexId = Nothing }
              updateStatePoseInfo $ PoseInfo.verifyPose problem newPose
    when (mb == GLFW.MouseButton'2 && mbs == GLFW.MouseButtonState'Pressed) $ do
      gets stateSelectedEdgeId >>= \case
        Nothing -> do
          let e = nearestEdge (x, y) poseInfo poseEdgeInfo
          modify $ \s -> s { stateSelectedEdgeId = Just (edgeId e) }
        Just _ -> do
          modify $ \s -> s { stateSelectedEdgeId = Nothing }
    draw

  EventCursorPos{} -> do
    (x, y) <- getCursorPos
    win    <- asks envWindow
    liftIO $ GLFW.setWindowTitle win $ "ICFPc 2021: cursor = " ++ show (x, y)

  (EventCursorEnter _ cs) -> do
    printEvent "cursor enter" [show cs]

  (EventScroll _ x y) -> do
    let x' = round x :: Int
        y' = round y :: Int
    printEvent "scroll" [show x', show y']

  (EventKey _win k scancode ks mk) -> do
    printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]

  (EventChar _ c) -> do
    printEvent "char" [show c]
    case c of
      'p' -> do
        poseInfo <- gets statePose
        liftIO $ BLC.putStrLn $ A.encode $ poseOfPoseInfo poseInfo
      'a' -> movePose (-1, 0, 1, 1)
      'd' -> movePose (1, 0, 1, 1)
      'w' -> movePose (0, -1, 1, 1)
      's' -> movePose (0, 1, 1, 1)
      'x' -> movePose (0, 0, -1, 1)
      'y' -> movePose (0, 0, 1, -1)
      'r' -> rotatePose
      'f' -> fold True
      'g' -> fold False
      't' -> tune
      '!' -> do
        probNum  <- optProblemNumber <$> asks envOptions
        poseInfo <- gets statePose
        liftIO $ do
          let jsonFn = printf "helper-%d.json" probNum
          BLC.writeFile jsonFn $ A.encode $ poseOfPoseInfo poseInfo
          putStrLn $ "Pose JSON saved: " ++ jsonFn
      'u' -> undo
      'i' -> redo
      _   -> pure ()
   where
    movePose (dx, dy, mx, my) = do
      problem     <- asks envProblem
      P.Pose b vs <- gets (poseOfPoseInfo . statePose)
      let vs' = [ P.Point (mx * (x + dx)) (my * (y + dy)) | P.Point x y <- vs ]
          newPose = P.Pose b vs'
          newPoseInfo = PoseInfo.verifyPose problem newPose
      updateStatePoseInfo newPoseInfo
      draw
    rotatePose = do
      problem     <- asks envProblem
      pose@(P.Pose b vs) <- gets (poseOfPoseInfo . statePose)
      let cp          = PoseInfo.centerPos pose
          vs'         = map (rotatePoint cp) vs
          newPose     = P.Pose b vs'
          newPoseInfo = PoseInfo.verifyPose problem newPose
      updateStatePoseInfo newPoseInfo
      draw
    fold b = gets stateSelectedEdgeId >>= \case
      Nothing  -> pure ()
      Just eid -> do
        problem  <- asks envProblem
        poseInfo <- gets statePose
        let
          (v1, v2) = edgeEndpoints poseInfo eid
          overTheEdge (P.Point x y) =
            let P.Point x1 y1 = v1
                P.Point x2 y2 = v2
            in  if b
                  then TwoDim.line ((x1, y1), (x2, y2)) (x, y) > 0
                  else TwoDim.line ((x1, y1), (x2, y2)) (x, y) < 0
          P.Pose bonus vs = poseOfPoseInfo poseInfo
          vs'             = flip map vs
            $ \v -> if overTheEdge v then foldPointAgainst (v1, v2) v else v
          newPose     = P.Pose bonus vs'
          newPoseInfo = PoseInfo.verifyPose problem newPose
        updateStatePoseInfo newPoseInfo
        draw
    tune = do
      problem <-asks envProblem
      state <- get
      let currentPose = poseOfPoseInfo (statePose state)
          initBk = Bk.mkBk problem
          bk = initBk{Bk.vertices=Map.fromList (zip [0..] $ map PoseInfo.pointToTuple $ P.pose'vertices currentPose)}
          bk' = Bk.autoTuneEdges bk
          newPose = currentPose{P.pose'vertices=map (\(x,y)->P.Point x y) $ Map.elems $ Bk.vertices bk'}
      updateStatePoseInfo (PoseInfo.verifyPose problem newPose)
      draw
    undo = gets stateUndoQueue >>= \case
      [] -> do
        liftIO $ putStrLn "empty undo queue"
      u : us -> do
        problem <- asks envProblem
        modify $ \s ->
          let currentPose = poseOfPoseInfo (statePose s)
          in  s { statePose      = PoseInfo.verifyPose problem u
                , stateUndoQueue = us
                , stateRedoQueue = currentPose : stateRedoQueue s
                }
        draw
    redo = gets stateRedoQueue >>= \case
      [] -> do
        liftIO $ putStrLn "empty redo queue"
      r : rs -> do
        problem <- asks envProblem
        modify $ \s ->
          let currentPose = poseOfPoseInfo (statePose s)
          in  s { statePose      = PoseInfo.verifyPose problem r
                , stateUndoQueue = currentPose : stateUndoQueue s
                , stateRedoQueue = rs
                }
        draw

rotatePoint :: P.Point -> P.Point -> P.Point
rotatePoint (P.Point cx cy) (P.Point x y) = P.Point (y-cy+cx) (-(x-cx)+cy)

-- ??????????????????????????????????????????
foldPointAgainst :: (P.Point, P.Point) -> P.Point -> P.Point
foldPointAgainst (p1, p2) p =
  let P.Point x1 y1 = p1
      P.Point x2 y2 = p2
      a = y2-y1
      b = - (x2-x1)
      c = - x1 * (y2 - y1) + y1 * (x2 - x1)
      P.Point x y = p
      x' = (- (a*a - b*b) * x - 2 * a * b * y - 2 * c * a) `div` (a*a + b*b)
      y' = (  (a*a - b*b) * y - 2 * a * b * x - 2 * b * c) `div` (a*a + b*b)
  in P.Point x' y'

nearestVertex :: (Int, Int) -> [PoseVertexInfo] -> PoseVertexInfo
nearestVertex (x, y) ps = minimumBy (compare `on` distance) ps
 where
  distance PoseVertexInfo { vertexPos = P.Point px py } =
    (x - px) ^ (2 :: Int) + (y - py) ^ (2 :: Int)

nearestEdge :: (Int, Int) -> PoseInfo -> [PoseEdgeInfo] -> PoseEdgeInfo
nearestEdge (x, y) poseInfo es = minimumBy (compare `on` distance) es
 where
  distance e =
    let P.Point ex ey = edgeMidPoint poseInfo (edgeId e)
    in  (x - ex) ^ (2 :: Int) + (y - ey) ^ (2 :: Int)

edgeEndpoints :: PoseInfo -> Int -> (P.Point, P.Point)
edgeEndpoints PoseInfo {..} eid =
  let (f, t)              = edgeFromTo (poseEdgeInfo !! eid)
      PoseVertexInfo _ p1 = poseVertexInfo !! f
      PoseVertexInfo _ p2 = poseVertexInfo !! t
  in  (p1, p2)

edgeMidPoint :: PoseInfo -> Int -> P.Point
edgeMidPoint poseInfo eid =
  let (P.Point x1 y1, P.Point x2 y2) = edgeEndpoints poseInfo eid
  in  P.Point ((x1 + x2) `div` 2) ((y1 + y2) `div` 2)

getCursorPos :: Demo (Int, Int)
getCursorPos = do
  win    <- asks envWindow
  sizeX  <- asks envCanvasSizeX
  sizeY  <- asks envCanvasSizeY
  width  <- gets stateWindowWidth
  height <- gets stateWindowHeight
  (x, y) <- liftIO $ GLFW.getCursorPos win
  let x' = round x * (2 * _MARGIN + sizeX) `div` width - _MARGIN
  let y' = round y * (2 * _MARGIN + sizeY) `div` height - _MARGIN
  pure (x' `div` _Bairitsu, y' `div` _Bairitsu)

adjustWindow :: Demo ()
adjustWindow = do
  state <- get
  sizeX <- asks envCanvasSizeX
  sizeY <- asks envCanvasSizeY
  let width  = stateFBWidth state
      height = stateFBHeight state
      pos    = GL.Position 0 0
      size   = GL.Size (fromIntegral width) (fromIntegral height)
  liftIO $ do
    GL.viewport GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GL.ortho (fromIntegral (-_MARGIN))
             (fromIntegral (sizeX + _MARGIN))
             (fromIntegral (sizeY + _MARGIN))
             (fromIntegral (-_MARGIN))
             (-1.5)
             (1.5 :: GL.GLdouble)
  draw

_MARGIN :: Int
_MARGIN = 10

envHole :: Env -> P.Hole
envHole Env { envProblem = P.Problem {..} } = hole

draw :: Demo ()
draw = do
  -- [CLI]
  poseInfo <- gets statePose
  mEdgeId <- gets stateSelectedEdgeId
  liftIO $ PoseInfo.reportPoseInfo_
    ( "    ",
      \case e | Just (edgeId e) == mEdgeId -> "[*] "
              | otherwise                  -> "[ ] " )
    poseInfo

  -- [CLI END]
  env <- ask
  let hole = envHole env
  state <- get
  liftIO $ do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    -- [hole]
    GL.color (GL.Color3 1 1 1 :: GL.Color3 GL.GLfloat)
    GL.LineSegments.lineWidth $= 3
    GL.renderPrimitive GL.LineLoop $ mapM_ (GL.vertex . toV) hole
    -- [hole:grid]
    when (optPrintGrid (envOptions env)) $ do
      GL.Points.pointSize $= 3
      GL.renderPrimitive GL.Points
        $ mapM_ (GL.vertex . toV) (Hole.innerPoints hole)
    -- [hole:vertex]
    GL.Points.pointSize $= 10
    GL.renderPrimitive GL.Points $ mapM_ (GL.vertex . toV) hole
    -- [bonuses]
    case P.bonuses (envProblem env) of
      Nothing      -> pure ()
      Just bonuses -> forM_ bonuses $ \P.BonusDef { position } -> do
        GL.Points.pointSize $= 10
        GL.color (GL.Color3 1 0 1 :: GL.Color3 GL.GLfloat)
        GL.renderPrimitive GL.Points $ GL.vertex (toV position)
  drawPose (envProblem env) (statePose state)
  liftIO $ GLFW.swapBuffers (envWindow env)

drawPose :: P.Problem -> PoseInfo -> Demo ()
drawPose P.Problem {..} PoseInfo {..} = do
  mId <- gets stateSelectedEdgeId
  forM_ poseEdgeInfo $ \PoseEdgeInfo {..} -> do
    let (min', max') = possibleLengthRange
        color | Just edgeId == mId  = (GL.Color3 0 1 1 :: GL.Color3 GL.GLfloat)
              | actualLength < min' = (GL.Color3 1 1 0 :: GL.Color3 GL.GLfloat)
              | actualLength > max' = (GL.Color3 1 0 0 :: GL.Color3 GL.GLfloat)
              | otherwise           = (GL.Color3 0 0 1 :: GL.Color3 GL.GLfloat)
    liftIO $ do
      GL.color color
      GL.renderPrimitive GL.Lines $ mapM_ GL.vertex (toE (edgePos edgeFromTo))
    gets stateSelectedVertexId >>= \case
      Nothing  -> pure ()
      Just vid -> do
        let v = poseVertexInfo !! vid
        liftIO $ do
          GL.color (GL.Color3 0 0 1 :: GL.Color3 GL.GLfloat)
          GL.Points.pointSize $= 10
          GL.renderPrimitive GL.Points $ GL.vertex (toV (vertexPos v))
 where
  edgePos (f, t) = (lookup' f, lookup' t)
  lookup' x = vertexPos (poseVertexInfo !! x)

toV :: P.Point -> GL.Vertex2 GL.GLdouble
toV (P.Point x y) =
  GL.Vertex2 (fromIntegral (_Bairitsu * x)) (fromIntegral (_Bairitsu * y))

toE :: (P.Point, P.Point) -> [GL.Vertex2 GL.GLdouble]
toE (s, e) = [toV s, toV e]

--------------------------------------------------------------------------------

printEvent :: String -> [String] -> Demo ()
printEvent cbname fields = do
  env <- ask
  when (optDumpEvents $ envOptions env) $ do
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk = "[mod keys: " ++ keys ++ "]"
 where
  keys = if null xs then "none" else unwords xs
  xs   = catMaybes ys
  ys =
    [ if GLFW.modifierKeysShift mk then Just "shift" else Nothing
    , if GLFW.modifierKeysControl mk then Just "control" else Nothing
    , if GLFW.modifierKeysAlt mk then Just "alt" else Nothing
    , if GLFW.modifierKeysSuper mk then Just "super" else Nothing
    ]

poseOfPoseInfo :: PoseInfo -> P.Pose
poseOfPoseInfo PoseInfo { poseVertexInfo } = P.Pose Nothing vs
  where vs = map vertexPos poseVertexInfo

updatePosOf :: Int -> P.Point -> P.Pose -> P.Pose
updatePosOf vertexId newPos (P.Pose b vs) =
  let (xs, _ : ys) = splitAt vertexId vs in P.Pose b $ xs ++ newPos : ys

updateStatePoseInfo :: PoseInfo -> Demo ()
updateStatePoseInfo poseInfo = do
  State { stateUndoQueue, statePose } <- get
  let currentPose  = poseOfPoseInfo statePose
      newUndoQueue = take 1000 $ currentPose : stateUndoQueue
  newUndoQueue' <- liftIO $ evaluate newUndoQueue -- ??????????????????
  modify $ \s -> s { statePose      = poseInfo
                   , stateUndoQueue = newUndoQueue'
                   , stateRedoQueue = []
                   }
