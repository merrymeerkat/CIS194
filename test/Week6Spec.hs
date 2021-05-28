module Week6Spec (spec) where

import Week6.HW6
import Test.Hspec

spec :: Spec
spec =
    describe "Week 6" $ do
        describe "fib" $ do
            it "should compute the nth fibonacci number (slowly)" $
                fib 5 `shouldBe` 5
        describe "fmap" $ do
            it "should map a function to all the elements in a stream" $
                fmap (+2) (sRepeat 0) `shouldBe` "[2, 2, 2, 2, 2, 2, 2, 2, 2, 2, ..."
        
        