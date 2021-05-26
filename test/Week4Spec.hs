module Week4Spec (spec) where

import Week4.HW4
import Test.Hspec

spec :: Spec
spec =
    describe "Week 4" $ do
        describe "==" $ do
            it "should compare two polynomials" $
                P [1,2,3] == P [1,2,3,0] `shouldBe` True
            it "should compare two polynomials" $
                P [1,2,3] == P [1,2,3,4] `shouldBe` False
        describe "show" $ do
            it "display a Poly type as a string" $
                show (P [(-1), 0, 5, 0, (-17)]) `shouldBe` "-17x^4 + 5x^2 + -1"
        describe "+" $ do
            it "add two polynomials" $
                P [1,2,0] + P[-5,4,0,9] `shouldBe` P [-4,6,0,9] 
        describe "*" $ do
            it "should multiply two polynomials" $
                P [1,2,0] * P[-5,4,0,9] `shouldBe` P [-5,-6,8,9,18]
            it "should multiply two polynomials" $
                P [0] * P[-5,4,0,9] `shouldBe` P [0]
            it "should multiply two polynomials" $
                P [1,2,0] * P[0] `shouldBe` P [0]
        describe "negate" $ do
            it "should negate a polynomial" $
                negate (P[-5,4,0,9]) `shouldBe` P [5,-4,0,-9]
        describe "fromInteger" $ do
            it "should convert an integer into a polynomial" $
                fromInteger 14 `shouldBe` P [14]
        describe "applyP" $ do
            it "should apply a polynomial to a value" $
                applyP (P[1,2,2]) 2 `shouldBe` 13
        describe "deriv" $ do
            it "should return the first derivative of a polynomial" $
                deriv (P[5,3,1]) `shouldBe` P [3,2]
        describe "nderiv" $ do
            it "should return the nth derivated of a polynomial" $
                nderiv 2 (P[5,3,1]) `shouldBe` P [2]
            it "should return the nth derivated of a polynomial" $
                nderiv 3 (P[5,3,1]) `shouldBe` P [0]
