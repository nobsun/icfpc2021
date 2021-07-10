{-# LANGUAGE DisambiguateRecordFields #-}
module Solver.SAT
  ( solve
  , unorderedPairs
  ) where

import Control.Monad
import Data.List (partition, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import System.IO
import Text.Printf

import qualified ToySolver.SAT as SAT

import qualified Parser as P
import qualified Hole


solve :: P.Problem -> IO P.Pose
solve prob = do
  hPrintf stderr "#vertices = %d\n" (length vs)
  hPrintf stderr "#edges = %d\n" (length es)
  hPrintf stderr "#innerPoints = %d\n" (length innerPoints)

  let ds = Map.fromList [(p1, sortOn snd [(p2, distance p1 p2) | p2 <- innerPoints]) | p1 <- innerPoints]

  hPutStrLn stderr "Adding variables .."
  solver <- SAT.newSolver
  xss <- liftM Map.fromList $ forM (zip [0..] (V.toList vs)) $ \(v, _) -> do
    xs <- forM innerPoints $ \p -> do
      var <- SAT.newVar solver
      return (p, var)
    SAT.addExactly solver (map snd xs) 1
    return (v, Map.fromList xs)

  hPutStrLn stderr "Adding edge constraints .."
  forM_ es $ \(P.Edge s t) -> do
    let orig_d = distance (vs V.! s) (vs V.! t)
        min_d = orig_d + ceiling (- fromIntegral orig_d * fromIntegral eps / 1000000 :: Rational)
        max_d = orig_d + floor (fromIntegral orig_d * fromIntegral eps / 1000000 :: Rational)
        xs_s = xss Map.! s
        xs_t = xss Map.! t
    forM_ (Map.toList xs_s) $ \(p_s, var_s) -> do
      let (shorter, tmp) = span ((< min_d) . snd) (ds Map.! p_s)
          (ok, longer) = span ((<= max_d) . snd) tmp
      forM_ (shorter ++ longer) $ \(p_t, _) -> do
        let var_t = xs_t Map.! p_t
        SAT.addClause solver [-var_s, -var_t]

  hPutStrLn stderr "Solving .."
  SAT.setLogger solver $ \s -> hPutStrLn stderr s
  ret <- SAT.solve solver
  m <- SAT.getModel solver

  let sol = P.Pose [head [p | (p, var) <- Map.toList xs, SAT.evalLit m var] | (v, i) <- zip (V.toList vs) [0..], let xs = xss Map.! i]
  return sol

  where
    hole = P.hole prob
    fig@(P.Figure{ P.edges = es, P.vertices = vs' }) = P.figure prob
    vs = V.fromList vs'
    -- es = P.edges fig
    eps = P.epsilon prob

    innerPoints = Hole.innerPoints hole


distance :: P.Point -> P.Point -> Int
distance (P.Point x1 y1) (P.Point x2 y2) = (x2 - x1)^(2::Int) + (y2 - y1)^(2::Int)


unorderedPairs :: [a] -> [(a, a)]
unorderedPairs [] = []
unorderedPairs xxs@(x:xs) = [(x,y) | y <- xxs] ++ unorderedPairs xs
