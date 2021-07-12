{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module PoseInfo where

import qualified Data.Set as Set
import Text.Printf (printf)

import Types (BonusType)
import qualified Hole
import qualified Parser                        as P
import           Parser                         ( Edge(..)
                                                , Figure(..)
                                                , Point(..), unPoint
                                                , BonusDef(..)
                                                , Pose(..)
                                                , Problem(..)
                                                )
import qualified Score
import qualified TwoDim

data PoseInfo = PoseInfo
  { poseVertexInfo :: [PoseVertexInfo]
  , poseEdgeInfo   :: [PoseEdgeInfo]
  , poseBonusInfo  :: [PoseBonusInfo]
  , poseDislikes   :: Int
  , poseIsValid    :: Bool
  , anyBonusUnlocked    :: Bool
  , allBonusUnlocked    :: Bool
  }
  deriving Show

data PoseVertexInfo = PoseVertexInfo
  { vertexId  :: Int
  , vertexPos :: P.Point
  }
  deriving Show

data PoseEdgeInfo = PoseEdgeInfo
  { edgeId :: Int
  , edgeFromTo :: (Int, Int)
  , actualLength :: Int
  , possibleLengthRange :: (Int, Int)
  , tolerant :: Bool
  , included :: Bool
  }
  deriving Show

data PoseBonusInfo = PoseBonusInfo
  { bonusPosition :: (Int, Int)
  , bonusType :: BonusType
  , bonusProblem :: Int
  , bonusUnlocked :: Bool
  }
  deriving Show

-- TODO rename
verifyPose :: P.Problem -> Pose -> PoseInfo
verifyPose Problem { bonuses, hole, figure, epsilon } pose@(Pose _bonus poseVertices) =
  let holeEdges = zip hole (tail hole ++ [head hole]) in
  if length (vertices figure) /= length poseVertices then
    error "Wrong number of edge"
  else do
    let poseVertexInfo =
          flip map (zip [0 :: Int ..] poseVertices) $ \(vertexId, vertexPos) ->
            PoseVertexInfo{..}
        poseEdgeInfo =
          flip map (zip [0 :: Int ..] (P.edges figure)) $ \(edgeId, Edge{s,e}) ->
            let edgeFromTo = (s,e)
                origEdge = (vertices figure !! s, vertices figure !! e)
                poseEdge = (poseVertices !! s, poseVertices !! e)
                actualLength  = Score.sqrDistance (edgeToTuple poseEdge)
                possibleLengthRange = Score.possibleLengthRange epsilon (edgeToTuple origEdge)
                tolerant = Score.tolerant epsilon (edgeToTuple origEdge) (edgeToTuple poseEdge)
                included =  and
                    [ fst poseEdge `Hole.isInsideHole` hole
                    , snd poseEdge `Hole.isInsideHole` hole
                    , not (any (intersect poseEdge) holeEdges)
                    ]
            in PoseEdgeInfo { .. }
        poseBonusInfo = maybe [] (map fromDef) bonuses
          where vertexSet = Set.fromList $ map unPoint poseVertices
                unlocked = (`Set.member` vertexSet)
                fromDef (BonusDef {position, bonus, problem}) =
                  let bonusPosition = unPoint position
                      bonusType = bonus
                      bonusProblem = problem
                      bonusUnlocked = unlocked bonusPosition
                  in PoseBonusInfo{..}
        poseDislikes = dislike hole pose
        poseIsValid = all valid poseEdgeInfo
          where valid PoseEdgeInfo{..} = tolerant && included
        anyBonusUnlocked = any bonusUnlocked poseBonusInfo
        allBonusUnlocked = all bonusUnlocked poseBonusInfo
      in PoseInfo {..}

{-# DEPRECATED reportPose "use reportPoseInfo" #-}
reportPose :: PoseInfo -> IO ()
reportPose = reportPoseInfo

reportPoseInfo :: PoseInfo -> IO ()
reportPoseInfo = reportPoseInfo_ ("", const "")

reportPoseInfo_ :: (String, PoseEdgeInfo -> String) -> PoseInfo -> IO ()
reportPoseInfo_ h poseInfo =
  mapM_ putStrLn $ pprPoseInfo h poseInfo

pprPoseInfo :: (String, PoseEdgeInfo -> String) -> PoseInfo -> [String]
pprPoseInfo (header, edgeHeader) PoseInfo{poseEdgeInfo, poseBonusInfo, poseDislikes, poseIsValid, anyBonusUnlocked, allBonusUnlocked} =
  [ header <> "edge     length   possible_range  tolerant included" ] ++
  map (pprEdgeInfo edgeHeader) poseEdgeInfo ++
  [ "position   bonus_type   problem unlocked" ] ++
  map pprBonusInfo poseBonusInfo ++
  [ "validPose: " <> show poseIsValid,
    "dislikes: " <> show poseDislikes,
    "anyUnlocked: " <> show anyBonusUnlocked <> " , allUnlocked: " <> show allBonusUnlocked ]

pprEdgeInfo :: (PoseEdgeInfo -> String) -> PoseEdgeInfo -> String
pprEdgeInfo header e@PoseEdgeInfo{..} =
  mconcat
  [ header e,
    printf "[%02d--%02d] " (fst edgeFromTo) (snd edgeFromTo),
    printf "%6d   " actualLength,
    printf "(%6d,%6d)    " (fst possibleLengthRange) (snd possibleLengthRange),
    printf "%s        " (mark tolerant),
    printf "%s" (mark included)]
 where
  mark True  = "✔"
  mark False = "✘"

pprBonusInfo :: PoseBonusInfo -> String
pprBonusInfo PoseBonusInfo{..} =
  mconcat
  [ printf "(%3d,%3d)  " (fst bonusPosition) (snd bonusPosition),
    printf "%-11s  " (show bonusType),
    printf "%6d  " bonusProblem,
    printf "   %s" (mark bonusUnlocked) ]
  where
    mark True  = "✔"
    mark False = "✘"

{-# ANN isValidPose "HLint: ignore Use &&" #-}
isValidPose :: P.Problem -> Pose -> Bool
isValidPose Problem { hole, figure, epsilon } (Pose _bonus poseVertices) =
  verticesNumCoincides && all validEdge (P.edges figure)
 where
  holeEdges            = zip hole (tail hole ++ [head hole])
  verticesNumCoincides = length (vertices figure) == length poseVertices
  validEdge Edge { s, e } = tolerant && included
   where
    origEdge = (vertices figure !! s, vertices figure !! e)
    poseEdge = (poseVertices !! s, poseVertices !! e)
    tolerant =
      Score.tolerant epsilon (edgeToTuple origEdge) (edgeToTuple poseEdge)
    included = and
      [ fst poseEdge `Hole.isInsideHole` hole
      , snd poseEdge `Hole.isInsideHole` hole
      , not (any (intersect poseEdge) holeEdges)
      ]

intersect :: (Point, Point) -> (Point, Point) -> Bool
intersect seg1 seg2 = TwoDim.intersect' (edgeToTuple seg1) (edgeToTuple seg2)

pointToTuple :: Point -> (Int, Int)
pointToTuple (Point x y) = (x, y)

edgeToTuple :: (Point, Point) -> ((Int, Int), (Int, Int))
edgeToTuple (s, e) = (pointToTuple s, pointToTuple e)

sqrDistance :: (Point, Point) -> Int
sqrDistance (Point x1 y1, Point x2 y2) = Score.sqrDistance ((x1, y1), (x2, y2))

dislike :: P.Hole -> P.Pose -> Int
dislike hole Pose{pose'vertices}= Score.dislike (hole',pose')
  where
    hole' = map pointToTuple hole
    pose' = map pointToTuple pose'vertices

centerPos :: P.Pose -> Point
centerPos Pose{pose'vertices} =
  P.Point (center (map P.x pose'vertices)) (center (map P.y pose'vertices))
  where
    center ps = sum ps `div` length ps
