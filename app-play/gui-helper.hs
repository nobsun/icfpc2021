{-# LANGUAGE ScopedTypeVariables #-} {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Exception
import Control.Monad ( foldM, liftM, unless, when, void, forM_ )
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put, gets)
import qualified Data.Aeson                as A
import Data.List                 (elemIndex)
import Data.Maybe                (catMaybes, isJust, fromJust, listToMaybe, fromMaybe)
import Data.IntMap.Lazy          (IntMap)
import qualified Data.IntMap.Lazy          as IntMap
import qualified Data.Map                  as Map
import Options.Applicative
import System.IO
import Text.Printf (printf)
import           Data.StateVar (($=))
import qualified Data.StateVar as StateVar

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.Points as GL.Points
import qualified Graphics.Rendering.OpenGL.GL.LineSegments as GL.LineSegments
import qualified Graphics.UI.GLFW          as GLFW

import qualified Solver.BackTracking       as Bk
import qualified Parser as P
import           Parser (Figure(..))
import           PoseInfo (PoseInfo (PoseInfo), PoseEdgeInfo(..), PoseVertexInfo (PoseVertexInfo, vertexId, vertexPos))
import qualified PoseInfo
import System.Exit (exitFailure)
import qualified Hole
import Data.Foldable (minimumBy)
import Data.Function (on)



--------------------------------------------------------- -----------------------

data Env = Env
    { envEventsChan :: TQueue Event
    , envWindow     :: !GLFW.Window
    , envOptions    :: Options
    , envProblem    :: P.Problem
    }

data State = State
    { stateWindowWidth      :: !Int
    , stateWindowHeight     :: !Int
    , stateFBWidth          :: !Int
    , stateFBHeight         :: !Int
    , statePoint            :: Maybe (Int,Int)
    , statePose             :: PoseInfo
    , stateDislike          :: !Int
    , stateSelectedVertexId :: Maybe Int
    , stateHistory          :: [(Int, Int)]
    -- , stateMouseBottonPressed :: Bool
    }

type Demo = RWST Env () State IO

sizeX, sizeY :: Int
sizeX = 100
sizeY = 100

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

data Options
  = Options
  { optHistory :: Maybe [(Int, Int)]
  , optDumpEvents :: Bool
  , optProblemNumber :: Int
  , optPose :: Maybe FilePath
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser = Options <$> history <*> dumpEvents <*> problemNumber <*> pose
  where
    history :: Parser (Maybe [(Int, Int)])
    history = optional $ option auto $ mconcat
      [ long "history"
      , metavar "STR"
      , help "history (type: [(Int,Int)])"
      ]

    dumpEvents :: Parser Bool
    dumpEvents = switch $ mconcat
      [ long "dump-events"
      , help "dump OpenGL events"
      ]

    problemNumber :: Parser Int
    problemNumber = option auto $ mconcat
      [ long "problem"
      , help "problem number"
      , metavar "NUM"
      , value 1
      ]

    pose :: Parser (Maybe FilePath)
    pose = optional $ strOption $ mconcat
      [ long "pose"
      , metavar "FILE"
      ]

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) fullDesc

main :: IO ()
main = do
    opt <- execParser parserInfo

    problem <- fromMaybe (error "parsing problem") <$>
      P.readProblem (printf "./data/problems/%03d.json" (optProblemNumber opt))
    mPose :: Maybe P.Pose <- case optPose opt of
      Nothing -> pure Nothing
      Just f -> fromMaybe (error "parsing pose") <$> A.decodeFileStrict' f

    let width  = 640
        height = 480

    eventsChan <- newTQueueIO :: IO (TQueue Event)

    withWindow width height "ICFPc 2021" $ \win -> do
        GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
        GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
        GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
        GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
        GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
        GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
        GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
        GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
        GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
        GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
        GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
        GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
        GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
        GLFW.setCharCallback            win $ Just $ charCallback            eventsChan

        GLFW.swapInterval 1

        (fbWidth, fbHeight) <- GLFW.getFramebufferSize win
        (winWidth, winHeight) <- GLFW.getWindowSize win

        let env = Env
              { envEventsChan    = eventsChan
              , envWindow        = win
              , envOptions       = opt
              , envProblem       = problem
              }
            h  = P.hole problem
            es = P.edges (P.figure problem)
            pose = case mPose of
              Nothing    -> P.Pose Nothing $ vertices (P.figure problem)
              Just pose' -> pose'
            state_ = State
              { stateWindowWidth      = winWidth
              , stateWindowHeight     = winHeight
              , stateFBWidth          = fbWidth
              , stateFBHeight         = fbHeight
              , statePoint            = Nothing
              , statePose             = PoseInfo.verifyPose problem pose
              , stateDislike          = 0
              , stateSelectedVertexId = Nothing
              , stateHistory          = []
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
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> Bool                                                             -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> Bool                                                             -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state =
    void $ evalRWST (adjustWindow >> run) env state

run :: Demo ()
run = do
    liftIO GLFW.waitEvents
    processEvents

    win <- asks envWindow
    q <- liftIO $ GLFW.windowShouldClose win
    if q then do
      state <- get
      liftIO $ hPutStrLn stderr $ "history: " ++ show (reverse (stateHistory state))
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
processEvent ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          win <- asks envWindow
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowPos _ x y) ->
          printEvent "window pos" [show x, show y]

      (EventWindowSize _ width height) -> do
          printEvent "window size" [show width, show height]
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          adjustWindow

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventWindowRefresh _) ->
          printEvent "window refresh" []

      (EventWindowFocus _ fs) ->
          printEvent "window focus" [show fs]

      (EventWindowIconify _ is) ->
          printEvent "window iconify" [show is]

      (EventFramebufferSize _ width height) -> do
          printEvent "framebuffer size" [show width, show height]
          modify $ \s -> s
            { stateFBWidth  = width
            , stateFBHeight = height
            }
          adjustWindow

      (EventMouseButton _ mb mbs mk) -> do
          printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
          poseInfo@PoseInfo{poseVertexInfo} <- gets statePose
          (x,y) <- getCursorPos
          when (mb == GLFW.MouseButton'1 && mbs == GLFW.MouseButtonState'Pressed) $ do
            gets stateSelectedVertexId >>= \case
              Nothing -> do
                let v =  nearestVertex (x,y) poseVertexInfo
                modify $ \s -> s
                  { stateSelectedVertexId = Just (vertexId v) }
              Just _ -> do
                pure ()
          when (mb == GLFW.MouseButton'1 && mbs == GLFW.MouseButtonState'Released) $ do
            gets stateSelectedVertexId >>= \case
              Nothing -> do
                pure ()
              Just vnum -> do
                let currentPose = poseOfPoseInfo poseInfo
                    newPose = updatePosOf vnum (P.Point x y) currentPose
                problem <- asks envProblem
                liftIO $ putStrLn $ "selected: " <> show vnum
                modify $ \s -> s
                  { statePose = PoseInfo.verifyPose problem newPose
                  , stateSelectedVertexId = Nothing
                  }
          draw

      (EventCursorPos _ x y) -> do
          (x,y) <- getCursorPos
          win <- asks envWindow
          poseInfo <- gets statePose
          liftIO $ GLFW.setWindowTitle win $ "ICFPc 2021: cursor = " ++ show (x, y)
          when False $ do -- 重すぎた。10回に一回とかにすればよい？
            gets stateSelectedVertexId >>= \case
              Nothing -> do
                pure ()
              Just vnum -> do
                let currentPose = poseOfPoseInfo poseInfo
                    newPose = updatePosOf vnum (P.Point x y) currentPose
                problem <- asks envProblem
                modify $ \s -> s
                  { statePose = PoseInfo.verifyPose problem newPose
                  , stateSelectedVertexId = Nothing
                  }
            draw

      (EventCursorEnter _ cs) -> do
          printEvent "cursor enter" [show cs]

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']

      (EventKey _win k scancode ks mk) -> do
          printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]

      (EventChar _ c) -> do
          pure ()
          -- state <- get
          -- let (dx,dy,mx,my) = case c of
          --              'a' -> (-1, 0, 1 ,1)
          --              'd' -> ( 1, 0, 1, 1)
          --              'w' -> ( 0,-1, 1, 1)
          --              's' -> ( 0, 1, 1, 1)
          --              'x' -> ( 0, 0,-1, 1)
          --              'y' -> ( 0, 0, 1,-1)
          --              _   -> ( 0, 0, 1, 1)
          --     P.Pose b vs = statePose state
          --     vs' = if c=='r' then map rotate vs
          --                     else [P.Point (mx*(x+dx)) (my*(y+dy)) | P.Point x y <-vs]
          -- modify $ \s -> s
          --   { statePose = P.Pose b vs'
          --   }
          -- printEvent "char" [show c]
          -- printEvent "vertices" [show vs']
          -- draw

nearestVertex :: (Int, Int) -> [PoseVertexInfo] -> PoseVertexInfo
nearestVertex (x,y) ps = minimumBy (compare `on` distance) ps
  where
    distance PoseVertexInfo{vertexPos=P.Point px py} =
      (x - px) ^ (2 :: Int) + (y - py) ^ (2 :: Int)

getCursorPos :: Demo (Int, Int)
getCursorPos = do
  win <- asks envWindow
  width <- gets stateWindowWidth
  height <- gets stateWindowHeight
  (x,y) <- liftIO $ GLFW.getCursorPos win
  let x' = ((round x-(width`div`2))*2*sizeX)`div`width
  let y' = ((round y-(height`div`2))*2*sizeY)`div`height-- because GL.ortho (-sizeX) (sizeX)
  pure (x', y')

rotate :: P.Point -> P.Point
rotate (P.Point x y) = P.Point y (-x)

nearElemIndex :: (Int,Int) -> [(Int,Int)] -> Maybe Int
nearElemIndex (x,y) as =
  listToMaybe $ catMaybes [(x+dx, y+dy)`elemIndex`as | dx<-[-2,-1..2], dy<-[-2,-1..2]]

adjustWindow :: Demo ()
adjustWindow = do
    state <- get
    let width  = stateFBWidth  state
        height = stateFBHeight state
        pos   = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
    liftIO $ do
        GL.viewport   GL.$= (pos, size)
        GL.matrixMode GL.$= GL.Projection
        GL.loadIdentity
        GL.ortho (fromIntegral(-sizeX)) (fromIntegral sizeX) (fromIntegral sizeY) (fromIntegral (-sizeY)) (-1.5) (1.5::GL.GLdouble)
    draw

envHole :: Env -> P.Hole
envHole Env{envProblem=P.Problem{..}} = hole

envEdges :: Env -> P.Edges
envEdges Env{envProblem=P.Problem{figure=P.Figure{..}}} = edges

draw :: Demo ()
draw = do
    liftIO . PoseInfo.reportPose =<< gets statePose
    env   <- ask
    let hole = envHole env
        edgeDef = envEdges env
    state <- get
    liftIO $ do
      GL.clear [GL.ColorBuffer, GL.DepthBuffer]
      -- [hole]
      GL.color (GL.Color3 1 1 1 :: GL.Color3 GL.GLfloat)
      GL.Points.pointSize $= 3
      GL.LineSegments.lineWidth $= 3
      GL.renderPrimitive GL.LineLoop $ mapM_ (GL.vertex . toV) hole
      GL.renderPrimitive GL.Points $ mapM_ (GL.vertex . toV) (Hole.innerPoints hole)
      -- [figure]
      drawPose (envProblem env) (statePose state)
      -- [end]
      GLFW.swapBuffers (envWindow env)

drawPose :: P.Problem -> PoseInfo -> IO ()
drawPose problem@P.Problem{..} PoseInfo{..} = do
  forM_ poseEdgeInfo $ \PoseEdgeInfo{..} -> do
    let (min',max') = possibleLengthRange
        color
          | actualLength < min' = (GL.Color3 1 1 0 :: GL.Color3 GL.GLfloat)
          | actualLength > max' = (GL.Color3 1 0 0 :: GL.Color3 GL.GLfloat)
          | otherwise           = (GL.Color3 0 0 1 :: GL.Color3 GL.GLfloat)
    GL.color color
    GL.renderPrimitive GL.Lines $ mapM_ GL.vertex (toE (edgePos edgeFromTo))
 where
  edgePos (f,t) = (lookup f, lookup t)
  lookup x = vertexPos (poseVertexInfo!!x)

toV :: P.Point -> GL.Vertex2 GL.GLdouble
toV (P.Point x y) = GL.Vertex2 (fromIntegral x) (fromIntegral y)

toE :: (P.Point, P.Point) -> [GL.Vertex2 GL.GLdouble]
toE (s,e) = [toV s, toV e]

edgeToV :: P.Vertices -> P.Edges -> [GL.Vertex2 GL.GLdouble]
edgeToV vertices edges =
  concat [[toV (vertices!!e1), toV (vertices!!e2)] | P.Edge e1 e2 <- edges]

--------------------------------------------------------------------------------

printEvent :: String -> [String] -> Demo ()
printEvent cbname fields = do
    env <- ask
    when (optDumpEvents $ envOptions env) $ do
      liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

poseOfPoseInfo :: PoseInfo -> P.Pose
poseOfPoseInfo PoseInfo{poseVertexInfo} = P.Pose Nothing vs
  where
    vs = map vertexPos poseVertexInfo

updatePosOf :: Int -> P.Point -> P.Pose -> P.Pose
updatePosOf vertexId newPos (P.Pose b vs) =
   let (xs,_:ys) = splitAt vertexId vs
   in P.Pose b $ xs ++ newPos : ys

