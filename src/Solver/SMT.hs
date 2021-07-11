{-# LANGUAGE BangPatterns #-}
module Solver.SMT
  ( solve
  , test
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Ratio
import qualified Data.Vector as V
import System.IO
import Text.Printf
import qualified Z3.Monad as Z3

import qualified Parser as P
import qualified Hole


solve :: P.Problem -> IO P.Pose
solve prob = do
  hPrintf stderr "#vertices = %d\n" (length vs)
  hPrintf stderr "#edges = %d\n" (length es)

  Z3.evalZ3 $ do
    zero <- Z3.mkIntNum (0 :: Int)
    one <- Z3.mkIntNum (1 :: Int)
    two <- Z3.mkIntNum (2 :: Int)

    pointVars <- liftM V.fromList $ forM (zip [(0::Int)..] (V.toList vs)) $ \(i, _) -> do
      x <- Z3.mkIntVar =<< Z3.mkStringSymbol ("x" ++ show i)
      y <- Z3.mkIntVar =<< Z3.mkStringSymbol ("y" ++ show i)
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

      let lim :: Integer
          lim = floor (sqrt (fromIntegral max_d + 0.001 :: Double))
      Z3.solverAssertCnstr =<< Z3.mkGe dx =<< Z3.mkIntNum (- lim)
      Z3.solverAssertCnstr =<< Z3.mkLe dx =<< Z3.mkIntNum lim
      Z3.solverAssertCnstr =<< Z3.mkGe dy =<< Z3.mkIntNum (- lim)
      Z3.solverAssertCnstr =<< Z3.mkLe dy =<< Z3.mkIntNum lim

      dx2 <- Z3.mkIntVar =<< Z3.mkStringSymbol ("e" ++ show i ++ "dx2")
      dy2 <- Z3.mkIntVar =<< Z3.mkStringSymbol ("e" ++ show i ++ "dy2")
      Z3.solverAssertCnstr =<< Z3.mkLe zero dx2
      Z3.solverAssertCnstr =<< Z3.mkLe zero dy2
      d <- Z3.mkAdd [dx2, dy2]
      min_d' <- Z3.mkIntNum min_d
      max_d' <- Z3.mkIntNum max_d
      Z3.solverAssertCnstr =<< Z3.mkLe min_d' d
      Z3.solverAssertCnstr =<< Z3.mkLe d max_d'

      return (dx,dy,dx2,dy2)

    forM_ (zip [0..] (V.toList vs)) $ \(i, _) -> do
       let (x', y') = pointVars V.! i

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
             lhs <- Z3.mkInt2Real x'
             rhs <- Z3.mkAdd =<< sequence [Z3.mkInt2Real x1', Z3.mkMul =<< sequence [Z3.mkInt2Real =<< Z3.mkSub [y', y1'], Z3.mkRational (fromIntegral (x2 - x1) % fromIntegral (y2 - y1))]]
             Z3.mkEq lhs rhs
         cond <- Z3.mkAnd [cond1, cond2]
         return cond

       cpTerms <- forM (zip hole (tail hole ++ [head hole])) $ \(P.Point x1 y1, P.Point x2 y2) -> do
         x1' <- Z3.mkIntNum x1
         y1' <- Z3.mkIntNum y1
         x2' <- Z3.mkIntNum x2
         y2' <- Z3.mkIntNum y2
         cond1 <- case compare y1 y2 of
                    EQ -> Z3.mkEq y1' y'
                    LT -> Z3.mkAnd =<< sequence [Z3.mkLe y1' y', Z3.mkLt y' y2']
                    GT -> Z3.mkAnd =<< sequence [Z3.mkLe y2' y', Z3.mkLt y' y1']
         cond2 <-
           if y1 == y2 then
             Z3.mkAnd =<< sequence (if x1 <= x2 then [Z3.mkLe x1' x', Z3.mkLe x' x2'] else [Z3.mkLe x2' x', Z3.mkLe x' x1'])
           else do
             lhs <- Z3.mkInt2Real x'
             rhs <- Z3.mkAdd =<< sequence [Z3.mkInt2Real x1', Z3.mkMul =<< sequence [Z3.mkInt2Real =<< Z3.mkSub [y', y1'], Z3.mkRational (fromIntegral (x2 - x1) % fromIntegral (y2 - y1))]]
             Z3.mkLt lhs rhs
         cond <- Z3.mkAnd [cond1, cond2]
         Z3.mkIte cond one zero

       cp <- Z3.mkAdd cpTerms
       condCP <- Z3.mkEq one =<< Z3.mkMod cp two
       Z3.solverAssertCnstr =<< Z3.mkOr (isOn ++ [condCP])

       return ()

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
            liftIO $ print sol

            actions <- liftM concat $ forM (zip [(0::Int)..] es) $ \(i, _) -> do
              let (dx', dy', dx2', dy2') = edgeVars V.! i
              Just dx <- Z3.evalInt model dx'
              Just dy <- Z3.evalInt model dy'
              Just dx2 <- Z3.evalInt model dx2'
              Just dy2 <- Z3.evalInt model dy2'
              return $ concat $
                [ [ do -- liftIO $ hPrintf stderr "%d^2 = %d < %d\n" dx (dx*dx) dx2
                       pre1 <- Z3.mkLe dx' =<< Z3.mkIntNum (abs dx)
                       pre2 <- Z3.mkGe dx' =<< Z3.mkIntNum (- abs dx)
                       pre <- Z3.mkAnd [pre1, pre2]
                       post <- Z3.mkLe dx2' =<< Z3.mkIntNum (dx * dx)
                       Z3.solverAssertCnstr =<< Z3.mkImplies pre post
                  | dx*dx < dx2 ]
                , [ do -- liftIO $ hPrintf stderr "%d^2 = %d > %d\n" dx (dx*dx) dx2
                       pre1 <- Z3.mkGe dx' =<< Z3.mkIntNum (abs dx)
                       pre2 <- Z3.mkLe dx' =<< Z3.mkIntNum (- abs dx)
                       pre <- Z3.mkOr [pre1, pre2]
                       post <- Z3.mkGe dx2' =<< Z3.mkIntNum (dx * dx)
                       Z3.solverAssertCnstr =<< Z3.mkImplies pre post
                  | dx*dx > dx2 ]
                , [ do -- liftIO $ hPrintf stderr "%d^2 = %d < %d\n" dy (dy*dy) dy2
                       pre1 <- Z3.mkLe dy' =<< Z3.mkIntNum (abs dy)
                       pre2 <- Z3.mkGe dy' =<< Z3.mkIntNum (- abs dy)
                       pre <- Z3.mkAnd [pre1, pre2]
                       post <- Z3.mkLe dy2' =<< Z3.mkIntNum (dy * dy)
                       Z3.solverAssertCnstr =<< Z3.mkImplies pre post
                  | dy*dy < dy2 ]
                , [ do -- liftIO $ hPrintf stderr "%d^2 = %d > %d\n" dy (dy*dy) dy2
                       pre1 <- Z3.mkGe dy' =<< Z3.mkIntNum (abs dy)
                       pre2 <- Z3.mkLe dy' =<< Z3.mkIntNum (- abs dy)
                       pre <- Z3.mkOr [pre1, pre2]
                       post <- Z3.mkGe dy2' =<< Z3.mkIntNum (dy * dy)
                       Z3.solverAssertCnstr =<< Z3.mkImplies pre post
                  | dy*dy > dy2 ]
                ]

            case actions of
              [] -> return $ Just sol
              _ : _ -> do
                sequence_ actions
                loop (k+1)

    ret <- loop 1
    case ret of
      Nothing -> error "should not happen"
      Just sol -> return $ P.Pose Nothing (V.toList sol)

  where
    hole = P.hole prob
    P.Figure{ P.edges = es, P.vertices = vs' } = P.figure prob
    vs = V.fromList vs'
    eps = P.epsilon prob


distance :: P.Point -> P.Point -> Int
distance (P.Point x1 y1) (P.Point x2 y2) = (x2 - x1)^(2::Int) + (y2 - y1)^(2::Int)


test :: IO ()
test = do
  forM_ [(1::Int)..59] $ \i -> do
    hPutStrLn stderr "==================================="
    let fname = printf "data/lightning-problems/%03d.json" i
    hPutStrLn stderr fname

    Just prob <- P.readProblem fname
    let P.Figure{ P.edges = es, P.vertices = vs' } = P.figure prob
        vs = V.fromList vs'
        eps = P.epsilon prob

    pose@P.Pose{ P.pose'vertices = ps } <- solve prob
    hPrint stderr pose

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