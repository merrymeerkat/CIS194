module week3Spec (spec) where

import week3
import Test.Hspec

spec :: Spec
spec =
    describe "Week 3" $ do
        describe "extend" $ do
            it "should extend a current state" $
                (extend empty "A" 5) A `shouldBe` 5
        describe "evalE" $ do
            it "should evaluate an expression given a state" $
                (evalE empty (Val 5)) `shouldBe` 5
            it "should evaluate an expression given a state" $
                (evalE empty (Op (Val 1) Eql (Val 2))) `shouldBe` 0
        describe "desugar" $ do
            it "should rewrite a statement as a `diet statement`" $
                (desugar (Incr "A")) `shouldBe` (Op (Var "A") Plus (Val 1))
        describe "evalSimple" $ do
            it "should evaluate a desugared statement" $
                (let s = evalSimple empty (DAssign "A" (Val 10)) in s "A" == 10) `shouldBe` True
            it "should evaluate a desugared statement" $
                (let s1 = evalSimple empty (DAssign "B" (Val 1)) in s "B" == 10) `shouldBe` False
        describe "run" $ do
            it "should run a programme that has not been desugared yet" $
                (run (extend empty "In" 4) factorial) "Out" `shouldBe` 24
            it "should run a programme that has not been desugared yet" $
                (run (extend empty "In" 7) fibonacci) "Out" `shouldBe` 21
            it "should run a programme that has not been desugared yet" $
                (run (extend empty "A" 36) squareRoot) "B" `shouldBe` 6

