module Week6Spec (spec) where

import Week6.HW6
import Test.Hspec

spec :: Spec
spec =
    describe "Week 6" $ do
        describe "fib" $ do
            it "should compute the nth fibonacci number (slowly)" $
                fib 5 `shouldBe` 5
        describe "minMax" $ do
            it "should return the minimum and maximum elements of a stream" $
                minMax [0,1,2,3456789] `shouldBe` Just (0, 3456789)
        describe "fastFib" $ do
            it "should compute the nth fibonacci number (quickly)" $
                fastFib 101 `shouldBe` 354224848179261915075 
