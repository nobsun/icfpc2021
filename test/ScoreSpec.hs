module ScoreSpec where

import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import qualified Score

spec :: Spec
spec = describe "dislike" $ do
  it "match with official judge for lambdaman" $ do
    let expected = Score.dislike (lambdamanHole, lambdamanSubmitPose)
        actual   = 4530
    expected `shouldBe` actual

lambdamanHole :: [(Int, Int)]
lambdamanHole =
  [ (55, 80) , (65, 95) , (95, 95) , (35, 5 ) , (5 , 5)
  , (35, 50) , (5 , 95) , (35, 95) , (45, 80)
  ]

-- ./solution/01.solve.02
lambdamanSubmitPose :: [(Int, Int)]
lambdamanSubmitPose =
  [ (56, 47) , (56, 37) , (26, 92) , (26, 32) , (36, 32) , (36, 62)
  , (36, 92) , (31, 22) , (31, 42) , (36, 32) , (46, 67) , (41, 22)
  , (41, 42) , (46, 32) , (51, 47) , (51, 77) , (21, 77) , (21, 87)
  , (36, 67) , (46, 67)
  ]
