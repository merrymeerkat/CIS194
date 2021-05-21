module Week4Spec (spec) where

import Week4.HW4
import Test.Hspec

spec :: Spec
spec =
    describe "Week 4" $ do
        describe "==" $ do
            it "should compare two polynomials" $
                (==) [1,2,3] [1,2,3,0] `shouldBe` True 

