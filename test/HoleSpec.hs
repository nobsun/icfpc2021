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

  describe "innerPoints" $ do
    it "returnes expected values" $ do
      length (innerPoints lambdaman) `shouldBe` 3836

lambdaman :: P.Hole
lambdaman = [P.Point x y | (x,y) <- [(55, 80), (65, 95), (95, 95), (35, 5), (5, 5), (35, 50), (5, 95), (35, 95), (45, 80)]]
