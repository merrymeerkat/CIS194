module Week1Spec (spec) where

import Week1.HW1 
import Test.Hspec

spec :: Spec
spec =
    describe "Week 1" $ do
        describe "lastDigit" $ do
            it "should return the last digit of a number" $
                lastDigit 15 `shouldBe` 5
        describe "dropLastDigit" $ do
            it "should drop the last digit of a number" $
                dropLastDigit 15 `shouldBe` 1
            it "should work with 0 too" $
                dropLastDigit 0 `shouldBe` 0
        describe "toRevDigits" $ do
            it "should break apart a number into a list of its digits in reverse order" $
                toRevDigits 1234 `shouldBe` [4,3,2,1]
            it "should return an empty list for n <= 0" $
                toRevDigits (-17) `shouldBe` []
        describe "doubleEveryOther" $ do
            it "should double every other number in a list, starting from the second" $
                doubleEveryOther [1,2,3,4] `shouldBe` [1,4,3,8]
        describe "sumDigits" $ do
            it "should calculate the sum of all the digits in every Integer in an Integer list" $
                sumDigits [10, 5, 18, 4] `shouldBe` 19
        describe "luhn" $ do
            it "should validate a credit card number" $
                luhn 4532456034700545 `shouldBe` True
            it "should validate a credit card number" $
                luhn 4532456034700546 `shouldBe` False

