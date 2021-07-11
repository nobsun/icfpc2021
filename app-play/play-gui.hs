{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Exception
import Control.Monad             (foldM, liftM, unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import qualified Data.Aeson                as A
import Data.List                 (elemIndex)
import Data.Maybe                (catMaybes, isJust, fromJust, listToMaybe, fromMaybe)
import Data.IntMap.Lazy          (IntMap)
import qualified Data.IntMap.Lazy          as IntMap
import qualified Data.Map                  as Map
import Options.Applicative
import System.IO
import Text.Printf (printf)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Types
import qualified Solver.BackTracking       as Bk
import qualified Parser as P
import           Parser (Figure(..))



--------------------------------------------------------- -----------------------

data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    , envOptions       :: Options
    }

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateFBWidth         :: !Int
    , stateFBHeight        :: !Int
    , statePoint           :: Maybe (Int,Int)
    , stateHole            :: [(Int, Int)]
    , stateEdges           :: [(Int, Int)]
    , stateVertices        :: [(Int, Int)]
    , stateEpsilon         :: !Int
    , stateDislike         :: !Int
    , stateSelectedNode    :: Maybe Int
    , stateBk              :: Bk.Bk
    , stateHistory         :: [(Int, Int)]
    }

type Demo = RWST Env () State IO

sizeX, sizeY :: Int
sizeX = 180
sizeY = 180

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
              }
            h  = map (\(P.Point x y) -> (x,y)) $ P.hole problem
            es = map (\(P.Edge  s e) -> (s,e)) $ P.edges (P.figure problem)
            vs = map (\(P.Point x y) -> (x,y)) $ case mPose of
              Nothing           -> vertices (P.figure problem)
              Just (P.Pose {P.pose'vertices = vs'}) -> vs'
            state_ = State
              { stateWindowWidth     = winWidth
              , stateWindowHeight    = winHeight
              , stateFBWidth         = fbWidth
              , stateFBHeight        = fbHeight
              , statePoint           = Nothing
              , stateHole            = h
              , stateEdges           = es
              , stateVertices        = vs
              , stateEpsilon         = 1
              , stateDislike         = 0
              , stateSelectedNode    = Nothing
              , stateBk              = Bk.mkBk problem
              , stateHistory         = []
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
          when (mb == GLFW.MouseButton'1 && mbs == GLFW.MouseButtonState'Released) $ do
              win <- asks envWindow
              (x,y) <- liftIO $ GLFW.getCursorPos win
              state <- get
              let width = stateWindowWidth state
                  height = stateWindowHeight state
                  x' = ((round x-(width`div`2))*2*sizeX)`div`width
                  y' = ((round y-(height`div`2))*2*sizeY)`div`height-- because GL.ortho (-sizeX) (sizeX)
                  bk = stateBk state
                  vs = stateVertices state
                  (sel,bk',vs') =
                      case (stateSelectedNode state, (x',y')`nearElemIndex`vs) of
                        (Nothing, Nothing) -> (Nothing, bk, vs)
                        (Nothing, Just n)  -> (Just n, bk, vs)
                        (Just n, _)  -> case Bk.move bk n (x',y') of
                                          Nothing -> (Nothing, bk, vs)
                                          Just bk'-> (Nothing, bk', Map.elems (Bk.vertices bk'))
              modify $ \s -> s
                { statePoint = Just (x', y')
                , stateSelectedNode = sel
                , stateBk = bk'
                , stateVertices = vs'
                }
              printEvent "mouse clicked" [show x', show y']
              printEvent "selected" [show sel]
              printEvent "vertices" [show vs']
              draw

      (EventCursorPos _ x y) -> do
          state <- get
          let width = stateWindowWidth state
              height = stateWindowHeight state
              x' = ((round x-(width`div`2))*2*sizeX)`div`width
              y' = ((round y-(height`div`2))*2*sizeY)`div`height
          --printEvent "cursor pos" [show x', show y']
          win <- asks envWindow
          liftIO $ GLFW.setWindowTitle win $ "ICFPc 2021: cursor = " ++ show (x', y')

      (EventCursorEnter _ cs) -> do
          printEvent "cursor enter" [show cs]

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']

      (EventKey _win k scancode ks mk) -> do
          printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]

      (EventChar _ c) -> do
          state <- get
          let (dx,dy,mx,my) = case c of
                       'a' -> (-1, 0, 1 ,1)
                       'd' -> ( 1, 0, 1, 1)
                       'w' -> ( 0,-1, 1, 1)
                       's' -> ( 0, 1, 1, 1)
                       'x' -> ( 0, 0,-1, 1)
                       'y' -> ( 0, 0, 1,-1)
                       _   -> ( 0, 0, 1, 1)
              vs = stateVertices state
              vs' = if c=='r' then map rotate vs
                              else [(mx*(x+dx),my*(y+dy)) | (x,y) <-vs]
          modify $ \s -> s
            { stateVertices = vs'
            }
          printEvent "char" [show c]
          printEvent "vertices" [show vs']
          draw

rotate :: (Int,Int) -> (Int,Int)
rotate (x, y) = (y, -x)

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

draw :: Demo ()
draw = do
    env   <- ask
    state <- get
    liftIO $ do
      GL.clear [GL.ColorBuffer, GL.DepthBuffer]
      GL.color (GL.Color3 1 1 1 :: GL.Color3 GL.GLfloat)
      GL.renderPrimitive GL.LineLoop $ mapM_ (GL.vertex . toV) (stateHole state)
      GL.color (GL.Color3 0.7 0 0 :: GL.Color3 GL.GLfloat)
      GL.renderPrimitive GL.Lines $ mapM_ GL.vertex (edgeToV (stateVertices state) (stateEdges state))
      GLFW.swapBuffers (envWindow env)

toV :: (Int,Int) -> GL.Vertex2 GL.GLdouble
toV (x,y) = GL.Vertex2 (fromIntegral x) (fromIntegral y)

edgeToV :: [(Int,Int)] -> [(Int,Int)] -> [GL.Vertex2 GL.GLdouble]
edgeToV vertices edges =
  concat [[toV (vertices!!e1), toV (vertices!!e2)] | (e1,e2) <- edges]

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
