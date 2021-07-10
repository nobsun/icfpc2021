module HoleSpec (spec) where

import Control.Monad
import Test.Hspec

import qualified Parser as P
import Hole

spec :: Spec
spec = do
  describe "isInsideHole" $ do
    it "contains all vertex points" $ do
      forM_ lambdaman $ \p -> do
        isInsideHole p lambdaman `shouldBe` True

    it "contains interior points" $ do
      let ps = [(35,5), (35,10), (35,50), (40,50), (35,75), (35,80), (35,85), (35,90), (35,95)]
      forM_ ps $ \(x,y) -> do
        isInsideHole (P.Point x y) lambdaman `shouldBe` True

    it "does not contain exterior points" $ do
      let ps =
            [ (4, 4), (5, 4), (35, 4), (45, 4), (55, 4), (65, 4), (95, 4)
            , (4, 100), (5, 100), (35, 100), (45, 100), (55, 100), (65, 100), (95, 100)
            , (4, 4), (4, 5), (4, 40), (4, 50), (4, 60), (4, 70), (4, 80), (4, 90), (4, 95), (4, 100)
            , (100, 4), (100, 5), (100, 40), (100, 50), (100, 60), (100, 70), (100, 80), (100, 90), (100, 95), (100, 100)
            , (6, 7), (34, 49), (34, 50), (44, 82)
            , (36, 96), (36, 6), (66, 50)
            ]
      forM_ ps $ \(x,y) -> do
        isInsideHole (P.Point x y) lambdaman `shouldBe` False

lambdaman :: P.Hole
lambdaman = [P.Point x y | (x,y) <- [(55, 80), (65, 95), (95, 95), (35, 5), (5, 5), (35, 50), (5, 95), (35, 95), (45, 80)]]

{-
testPos1 = and [isInsideHole p lambdaman | p <- lambdaman]

testPos2 = and
  [ isInsideHole (P.Point 35 5) lambdaman
  , isInsideHole (P.Point 35 10) lambdaman
  , isInsideHole (P.Point 35 50) lambdaman
  , isInsideHole (P.Point 40 50) lambdaman
  , isInsideHole (P.Point 35 75) lambdaman
  , isInsideHole (P.Point 35 80) lambdaman
  , isInsideHole (P.Point 35 85) lambdaman
  , isInsideHole (P.Point 35 90) lambdaman
  , isInsideHole (P.Point 35 95) lambdaman

  , isInsideHole (P.Point 6 6) lambdaman
  , not $ isInsideHole (P.Point 34 48) lambdaman
  ]

-- Lambdaman
testNeg = and
  [ not $ isInsideHole (P.Point 4 4) lambdaman
  , not $ isInsideHole (P.Point 5 4) lambdaman
  , not $ isInsideHole (P.Point 35 4) lambdaman
  , not $ isInsideHole (P.Point 45 4) lambdaman
  , not $ isInsideHole (P.Point 55 4) lambdaman
  , not $ isInsideHole (P.Point 65 4) lambdaman
  , not $ isInsideHole (P.Point 95 4) lambdaman

  , not $ isInsideHole (P.Point 4 100) lambdaman
  , not $ isInsideHole (P.Point 5 100) lambdaman
  , not $ isInsideHole (P.Point 35 100) lambdaman
  , not $ isInsideHole (P.Point 45 100) lambdaman
  , not $ isInsideHole (P.Point 55 100) lambdaman
  , not $ isInsideHole (P.Point 65 100) lambdaman
  , not $ isInsideHole (P.Point 95 100) lambdaman

  , not $ isInsideHole (P.Point 4 4) lambdaman
  , not $ isInsideHole (P.Point 4 5) lambdaman
  , not $ isInsideHole (P.Point 4 40) lambdaman
  , not $ isInsideHole (P.Point 4 50) lambdaman
  , not $ isInsideHole (P.Point 4 60) lambdaman
  , not $ isInsideHole (P.Point 4 70) lambdaman
  , not $ isInsideHole (P.Point 4 80) lambdaman
  , not $ isInsideHole (P.Point 4 90) lambdaman
  , not $ isInsideHole (P.Point 4 95) lambdaman
  , not $ isInsideHole (P.Point 4 100) lambdaman

  , not $ isInsideHole (P.Point 100 4) lambdaman
  , not $ isInsideHole (P.Point 100 5) lambdaman
  , not $ isInsideHole (P.Point 100 40) lambdaman
  , not $ isInsideHole (P.Point 100 50) lambdaman
  , not $ isInsideHole (P.Point 100 60) lambdaman
  , not $ isInsideHole (P.Point 100 70) lambdaman
  , not $ isInsideHole (P.Point 100 80) lambdaman
  , not $ isInsideHole (P.Point 100 90) lambdaman
  , not $ isInsideHole (P.Point 100 95) lambdaman
  , not $ isInsideHole (P.Point 100 100) lambdaman

  , not $ isInsideHole (P.Point 6 7) lambdaman
  , not $ isInsideHole (P.Point 34 49) lambdaman
  , not $ isInsideHole (P.Point 34 50) lambdaman
  , not $ isInsideHole (P.Point 44 82) lambdaman  
  , not $ isInsideHole (P.Point 36 96) lambdaman
  , not $ isInsideHole (P.Point 36 6) lambdaman
  , not $ isInsideHole (P.Point 66 50) lambdaman
  ] 

-}
