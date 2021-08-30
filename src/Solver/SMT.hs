{-# LANGUAGE BangPatterns #-}
module Solver.SMT
  ( solve
  , solveWith
  , Options (..)

  , solveFor, Session (..)
  , test
  ) where

import Control.Monad
import Control.Monad.Trans
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default.Class
import Data.Maybe
import Data.Monoid
import Data.Ratio
import qualified Data.Set as Set
import qualified Data.Vector as V
import System.IO
import Text.Printf
import qualified Z3.Monad as Z3

import qualified Parser as P
import qualified PoseInfo
import qualified Hole


setParam :: Z3.MonadZ3 m => Options -> m ()
setParam opt = do
  params <- Z3.mkParams
  case optThreads opt of
    Nothing -> return ()
    Just ths -> do
      threads <- Z3.mkStringSymbol "threads"
      liftIO $ putStrLn $ "solver-param threads: " ++ show ths
      Z3.paramsSetUInt params threads ths
  Z3.solverSetParams params

solve :: P.Problem -> IO P.Pose
solve = solveWith def

data Options
  = Options
  { optThreads :: Maybe Word
  , optZeroDislikes :: Bool
  , optGetBonus :: [Int]
  }
  deriving (Eq, Show)

instance Default Options where
  def =
    Options
    { optThreads = Nothing
    , optZeroDislikes = False
    , optGetBonus = []
    }

solveWith :: Options -> P.Problem -> IO P.Pose
solveWith opt prob = do
  hPrintf stderr "#vertices = %d\n" (length vs)
  hPrintf stderr "#edges = %d\n" (length es)

  Z3.evalZ3 $ do
    setParam opt

    pointVars <- liftM V.fromList $ forM (zip [(0::Int)..] (V.toList vs)) $ \(i, _) -> do
      x <- Z3.mkIntVar =<< Z3.mkStringSymbol ("x" ++ show i)
      y <- Z3.mkIntVar =<< Z3.mkStringSymbol ("y" ++ show i)
      x' <- Z3.mkInt2Real x
      y' <- Z3.mkInt2Real y
      assertIsInside (x',y') hole
      return (x,y)

    edgeVars <- liftM V.fromList $ forM (zip [(0::Int)..] es) $ \(i, P.Edge s t) -> do
      let orig_d = distance (vs V.! s) (vs V.! t)
      let min_d = orig_d + ceiling (- fromIntegral orig_d * fromIntegral eps / 1000000 :: Rational)
      let max_d = orig_d + floor (fromIntegral orig_d * fromIntegral eps / 1000000 :: Rational)

      dx <- Z3.mkIntVar =<< Z3.mkStringSymbol ("e" ++ show i ++ "dx")
      dy <- Z3.mkIntVar =<< Z3.mkStringSymbol ("e" ++ show i ++ "dy")
      let (sx, sy) = pointVars V.! s
          (tx, ty) = pointVars V.! t
      Z3.solverAssertCnstr =<< Z3.mkEq tx =<< Z3.mkAdd [sx, dx]
      Z3.solverAssertCnstr =<< Z3.mkEq ty =<< Z3.mkAdd [sy, dy]

      let lim :: Int
          lim = floor (sqrt (fromIntegral max_d + 0.001 :: Double))
      -- liftIO $ print (i, min_d, max_d, lim)
      Z3.solverAssertCnstr =<< Z3.mkGe dx =<< Z3.mkIntNum (- lim)
      Z3.solverAssertCnstr =<< Z3.mkLe dx =<< Z3.mkIntNum lim
      Z3.solverAssertCnstr =<< Z3.mkGe dy =<< Z3.mkIntNum (- lim)
      Z3.solverAssertCnstr =<< Z3.mkLe dy =<< Z3.mkIntNum lim

      dx2 <- Z3.mkIntVar =<< Z3.mkStringSymbol ("e" ++ show i ++ "dx2")
      dy2 <- Z3.mkIntVar =<< Z3.mkStringSymbol ("e" ++ show i ++ "dy2")
      assertIsSquare dx dx2 lim
      assertIsSquare dy dy2 lim
      d <- Z3.mkAdd [dx2, dy2]
      min_d' <- Z3.mkIntNum min_d
      max_d' <- Z3.mkIntNum max_d
      Z3.solverAssertCnstr =<< Z3.mkLe min_d' d
      Z3.solverAssertCnstr =<< Z3.mkLe d max_d'

      return (dx,dy,dx2,dy2)

    when (optZeroDislikes opt) $ do
      forM_ hole $ \(P.Point x y) -> do
        x' <- Z3.mkIntNum x
        y' <- Z3.mkIntNum y
        Z3.solverAssertCnstr =<< Z3.mkOr =<< sequence [Z3.mkAnd =<< sequence [Z3.mkEq x' x2', Z3.mkEq y' y2'] | (x2',y2') <- V.toList pointVars]

    forM_ (optGetBonus opt) $ \i -> do
      let P.Point x y = P.position $ fromMaybe [] (P.bonuses prob) !! i
      x' <- Z3.mkIntNum x
      y' <- Z3.mkIntNum y
      Z3.solverAssertCnstr =<< Z3.mkOr =<< sequence [Z3.mkAnd =<< sequence [Z3.mkEq x' x2', Z3.mkEq y' y2'] | (x2',y2') <- V.toList pointVars]

    let loop :: Int -> Z3.Z3 (Maybe (V.Vector P.Point))
        loop !k = do
          liftIO $ hPrintf stderr "Solving (%d) ..\n" k
          ret <- Z3.solverCheck
          liftIO $ print ret
          if ret /= Z3.Sat then do
            return Nothing
          else do
            model <- Z3.solverGetModel
            sol <- V.forM pointVars $ \(x',y') -> do
              Just x <- Z3.evalInt model x'
              Just y <- Z3.evalInt model y'
              return $ P.Point (fromIntegral x) (fromIntegral y)

            let pose = P.Pose Nothing (V.toList sol)
                info = PoseInfo.verifyPose prob pose
            liftIO $ do
              hFlush stderr
              BL.putStrLn $ JSON.encode pose
              PoseInfo.reportPoseInfo info
              hFlush stdout

            constrs <- liftM concat $ forM (zip [(0::Int)..] es) $ \(i, _) -> do
              let (dx', dy', dx2', dy2') = edgeVars V.! i
              liftM catMaybes $ sequence
                [ encodeIsSquareBlocking model dx' dx2'
                , encodeIsSquareBlocking model dy' dy2'
                ]

            case constrs of
              (_ : _) -> do
                mapM_ Z3.solverAssertCnstr constrs
                loop (k+1)
              [] -> do
                actions2 <- liftM concat $ forM es $ \(P.Edge s t) -> do
                  let p1@(P.Point x1 y1) = sol V.! s
                      p2@(P.Point x2 y2) = sol V.! t
                      rs1 = catMaybes [findIntersectionRatio (p1,p2) (p3,p4) | (p3, p4) <- zip hole (tail hole ++ [head hole]), intersect' (p1,p2) (p3,p4)]
                      rs2 = Set.fromList $ [0, 1] ++ rs1
                      rs3 = Set.toList $ Set.union rs2 (Set.fromList (zipWith (\r1 r2 -> (r1 + r2) / 2) (Set.toList rs2) (tail (Set.toList rs2))))
                  case listToMaybe [(r, p) | r <- rs3, let p = mix r (x1,y1) (x2,y2), not (isInsideHole p hole)] of
                    Nothing -> return []
                    Just (r, p) -> do
                      let (x1', y1') = pointVars V.! s
                          (x2', y2') = pointVars V.! t
                      x1'' <- Z3.mkInt2Real x1'
                      y1'' <- Z3.mkInt2Real y1'
                      x2'' <- Z3.mkInt2Real x2'
                      y2'' <- Z3.mkInt2Real y2'
                      (x'', y'') <- mix' r (x1'', y1'') (x2'', y2'')
                      return [liftIO (print (P.Edge s t, p1, p2, p, r)) >> assertIsInside (x'', y'') hole]

                case actions2 of
                  (_ : _) -> do
                    sequence_ actions2
                    loop (k+1)
                  [] -> return $ Just sol

    ret <- loop 1
    case ret of
      Nothing -> error "should not happen"
      Just sol -> return $ P.Pose Nothing (V.toList sol)

  where
    hole = P.hole prob
    P.Figure{ P.edges = es, P.vertices = vs' } = P.figure prob
    vs = V.fromList vs'
    eps = P.epsilon prob


assertIsSquare :: Z3.AST -> Z3.AST -> Int -> Z3.Z3 ()
assertIsSquare x x2 lim = do
  zero <- Z3.mkIntNum (0 :: Int)
  Z3.solverAssertCnstr =<< Z3.mkGe x2 zero
  Z3.solverAssertCnstr =<< Z3.mkLe x2 =<< Z3.mkIntNum (lim*lim)
  forM_ [0..lim] $ \n -> do
    lb <- Z3.mkIntNum (- n)
    ub <- Z3.mkIntNum n
    pre1 <- Z3.mkAnd =<< sequence [Z3.mkLe lb x, Z3.mkLe x ub]
    post1 <- Z3.mkLe x2 =<< Z3.mkIntNum (n*n)
    Z3.solverAssertCnstr =<< Z3.mkImplies pre1 post1
    pre2 <- Z3.mkOr =<< sequence [Z3.mkGe lb x, Z3.mkGe x ub]
    post2 <- Z3.mkGe x2 =<< Z3.mkIntNum (n*n)
    Z3.solverAssertCnstr =<< Z3.mkImplies pre2 post2


-- x2 が x の二乗でない場合に、その反例をブロックする制約条件を返す
encodeIsSquareBlocking :: Z3.Model -> Z3.AST -> Z3.AST -> Z3.Z3 (Maybe Z3.AST)
encodeIsSquareBlocking model x x2 = do
  Just v <- Z3.evalInt model x
  Just v2 <- Z3.evalInt model x2
  case compare (v*v) v2 of
    EQ -> return Nothing
    LT -> do
      liftIO $ putStrLn "A"
      pre1 <- Z3.mkLe x =<< Z3.mkIntNum (abs v)
      pre2 <- Z3.mkGe x =<< Z3.mkIntNum (- abs v)
      pre <- Z3.mkAnd [pre1, pre2]
      post <- Z3.mkLe x2 =<< Z3.mkIntNum (v * v)
      liftM Just $ Z3.mkImplies pre post
    GT -> do
      liftIO $ putStrLn "B"
      pre1 <- Z3.mkGe x =<< Z3.mkIntNum (abs v)
      pre2 <- Z3.mkLe x =<< Z3.mkIntNum (- abs v)
      pre <- Z3.mkOr [pre1, pre2]
      post <- Z3.mkGe x2 =<< Z3.mkIntNum (v * v)
      liftM Just $ Z3.mkImplies pre post


assertIsInside :: (Z3.AST, Z3.AST) -> P.Hole -> Z3.Z3 ()
assertIsInside (x', y') hole = do
  isOn <- forM (zip hole (tail hole ++ [head hole])) $ \(P.Point x1 y1, P.Point x2 y2) -> do
    x1' <- Z3.mkIntNum x1
    y1' <- Z3.mkIntNum y1
    x2' <- Z3.mkIntNum x2
    y2' <- Z3.mkIntNum y2
    cond1 <- case compare y1 y2 of
               EQ -> Z3.mkEq y1' y'
               LT -> Z3.mkAnd =<< sequence [Z3.mkLe y1' y', Z3.mkLe y' y2']
               GT -> Z3.mkAnd =<< sequence [Z3.mkLe y2' y', Z3.mkLe y' y1']
    cond2 <-
      if y1 == y2 then
        Z3.mkAnd =<< sequence (if x1 <= x2 then [Z3.mkLe x1' x', Z3.mkLe x' x2'] else [Z3.mkLe x2' x', Z3.mkLe x' x1'])
      else do
        rhs <- Z3.mkAdd =<< sequence [pure x1', Z3.mkMul =<< sequence [Z3.mkSub [y', y1'], Z3.mkRational (fromIntegral (x2 - x1) % fromIntegral (y2 - y1))]]
        Z3.mkEq x' rhs
    cond <- Z3.mkAnd [cond1, cond2]
    return cond

  cps <- liftM catMaybes $ forM (zip hole (tail hole ++ [head hole])) $ \(P.Point x1 y1, P.Point x2 y2) -> do
    x1' <- Z3.mkIntNum x1
    y1' <- Z3.mkIntNum y1
    y2' <- Z3.mkIntNum y2
    if y1 == y2 then
      return Nothing
    else do
      cond1 <-
        if y1 < y2 then
          Z3.mkAnd =<< sequence [Z3.mkLe y1' y', Z3.mkLt y' y2']
        else
          Z3.mkAnd =<< sequence [Z3.mkLe y2' y', Z3.mkLt y' y1']
      rhs <- Z3.mkAdd =<< sequence [pure x1', Z3.mkMul =<< sequence [Z3.mkSub [y', y1'], Z3.mkRational (fromIntegral (x2 - x1) % fromIntegral (y2 - y1))]]
      cond2 <- Z3.mkLt x' rhs
      liftM Just $ Z3.mkAnd [cond1, cond2]

  condCP <-
    -- 和が奇数であることよりも、xorを使った方が効率が良い?
    if False then do
      zero <- Z3.mkIntNum (0 :: Int)
      one <- Z3.mkIntNum (1 :: Int)
      two <- Z3.mkIntNum (2 :: Int)
      cp <- Z3.mkAdd =<< sequence [Z3.mkIte cond one zero | cond <- cps]
      Z3.mkEq one =<< Z3.mkMod cp two
    else do
      false <- Z3.mkFalse
      foldM Z3.mkXor false cps

  Z3.solverAssertCnstr =<< Z3.mkOr (isOn ++ [condCP])


distance :: P.Point -> P.Point -> Int
distance (P.Point x1 y1) (P.Point x2 y2) = (x2 - x1)^(2::Int) + (y2 - y1)^(2::Int)


mix :: Integral a => Rational -> (a,a) -> (a,a) -> (Rational, Rational)
mix r (x1,y1) (x2,y2) = (r * fromIntegral x1 + (1 - r) * fromIntegral x2, r * fromIntegral y1 + (1 - r) * fromIntegral y2)


mix' :: Rational -> (Z3.AST, Z3.AST) -> (Z3.AST, Z3.AST) -> Z3.Z3 (Z3.AST, Z3.AST)
mix' r (x1,y1) (x2,y2) = do
  r' <- Z3.mkRational r
  s' <- Z3.mkRational (1 - r)
  x <- Z3.mkAdd =<< sequence [Z3.mkMul [r', x1], Z3.mkMul [s', x2]]
  y <- Z3.mkAdd =<< sequence [Z3.mkMul [r', y1], Z3.mkMul [s', y2]]
  return (x,y)


-- http://www5d.biglobe.ne.jp/~tomoya03/shtml/algorithm/Intersection.htm
-- この記事と異なり、接するだけの場合も True を返すようにしている
intersect' :: (P.Point, P.Point) -> (P.Point, P.Point) -> Bool
intersect' (P.Point x1 y1, P.Point x2 y2) (P.Point x3 y3, P.Point x4 y4) = ta*tb <= 0 && tc*td <= 0
  where
    ta = (x3-x4)*(y1-y3)+(y3-y4)*(x3-x1)
    tb = (x3-x4)*(y2-y3)+(y3-y4)*(x3-x2)
    tc = (x1-x2)*(y3-y1)+(y1-y2)*(x1-x3)
    td = (x1-x2)*(y4-y1)+(y1-y2)*(x1-x4)


-- https://qiita.com/kaityo256/items/988bf94bf7b674b8bfdc
findIntersectionRatio :: (P.Point, P.Point) -> (P.Point, P.Point) -> Maybe Rational
findIntersectionRatio (p1@(P.Point x1 y1), p2@(P.Point x2 y2)) (P.Point x3 y3, P.Point x4 y4)
  | p1 == p2 = Just 1
  | det == 0 = Nothing -- error ("should not happen: " ++ show ((P.Point x1 y1, P.Point x2 y2), (P.Point x3 y3, P.Point x4 y4)))
  | otherwise = Just t
  where
    det = (x1 - x2) * (y4 - y3) - (x4 - x3) * (y1 - y2)
    t = fromIntegral ((y4 - y3) * (x4 - x2) + (x3 - x4) * (y4 - y2)) % fromIntegral det


-- Hole.hs からコピペして、点の座標を Point から (Rational, Rational) に変更
isInsideHole :: (Rational, Rational) -> P.Hole -> Bool
isInsideHole p h =
  case getAp m of
    Nothing -> True
    Just cp -> odd (getSum cp :: Int)
  where
    m = mconcat
      [ if isOn p e then
          Ap Nothing
        else if isCrossing p e then
          Ap (Just 1)
        else
          Ap (Just 0)
      | e <- zip h (tail h ++ [head h])
      ]


-- Hole.hs からコピペして、点の座標を Point から (Rational, Rational) に変更
isOn :: (Rational, Rational) -> (P.Point, P.Point) -> Bool
isOn (x, y) (P.Point x1 y1, P.Point x2 y2)
  | not (y1' <= y && y <= y2') && not (y2' <= y && y <= y1') = False
  | y1' == y2' = x1' <= x && x <= x2' || x2' <= x && x <= x1'
  | otherwise = x == x1' + (y - y1') * ((x2' - x1') / (y2' - y1'))
  where
    x1' = fromIntegral x1
    x2' = fromIntegral x2
    y1' = fromIntegral y1
    y2' = fromIntegral y2


-- Hole.hs からコピペして、点の座標を Point から (Rational, Rational) に変更
-- https://www.nttpc.co.jp/technology/number_algorithm.html
isCrossing :: (Rational, Rational) -> (P.Point, P.Point) -> Bool
isCrossing (x, y) (P.Point x1 y1, P.Point x2 y2) =
  ((y1' <= y && y < y2') || (y2' <= y && y < y1')) &&
  x < x1' + (y - y1') * ((x2' - x1') / (y2' - y1'))
  where
    x1' = fromIntegral x1
    x2' = fromIntegral x2
    y1' = fromIntegral y1
    y2' = fromIntegral y2

data Session = Lightning | Main
instance Show Session where
  show Lightning = "lightning-problems"
  show Main      = "problems"

solveFor :: Options -> Session -> Int -> IO ()
solveFor opt sess i = do
  hPutStrLn stderr "==================================="
  let fname = printf "data/%s/%03d.json" (show sess) i
  hPutStrLn stderr fname

  Just prob <- P.readProblem fname
  let P.Figure{ P.edges = es, P.vertices = vs' } = P.figure prob
      vs = V.fromList vs'
      eps = P.epsilon prob

  pose@P.Pose{ P.pose'vertices = ps } <- solveWith opt prob
  JSON.encodeFile (printf "sol%03d.json" i) pose

  let hole = P.hole prob
  forM_ ps $ \p -> do
    unless (Hole.isInsideHole p hole) $ do
      hPrintf stderr "%d is not inside hole\n" (show p)

  forM_ es $ \e@(P.Edge s t) -> do
    let orig_d = distance (vs V.! s) (vs V.! t)
    let min_d = orig_d + ceiling (- fromIntegral orig_d * fromIntegral eps / 1000000 :: Rational)
    let max_d = orig_d + floor (fromIntegral orig_d * fromIntegral eps / 1000000 :: Rational)
    let d = distance (ps !! s) (ps !! t)
    unless (min_d <= d && d <= max_d) $ do
      hPrintf stderr "(%s) (length %d) is mapped to (%s, %s) (length %d)\n" (show e) orig_d (show (ps !! s)) (show (ps !! t)) d
      hPrintf stderr "But %d is not in [%d, %d]\n" d min_d max_d

test :: IO ()
test = forM_ [(1::Int)..59] (solveFor def Lightning)
