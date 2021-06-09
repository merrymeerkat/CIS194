module Week7Spec (spec) where

import Week7.HW7
import Week7.Cards
import Test.Hspec
import qualified Data.Vector as V

spec :: Spec
spec =
    describe "Week 7" $ do
        describe "liftM'" $ do
            it "should lift a regular function into a monad and apply it to an argument in that monad" $
                liftM' (+1) (Just 5) `shouldBe` Just 6
        describe "swapV" $ do
            it "should, given two indices, safely swap the elements at those indexes in some vector" $
                swapV 0 2 (V.fromList [1,2,3]) `shouldBe` Just (V.fromList [3,2,1])
            it "should return Nothing if any index is out of range" $
                swapV 0 2 (V.fromList [1,2]) `shouldBe` Nothing
        describe "mapM'" $ do
            it "should map a monadic function across a list" $
                mapM' Just [1..10] `shouldBe` Just [1..10]
        describe "getElts" $ do
            it "should take in a list of indices and a vector and return (maybe) a list of the elements at those indices" $
                getElts [1,3] (V.fromList [0..9]) `shouldBe` Just [1,3]
            it "should return Nothing if any index is out of range" $
                getElts [1,10] (V.fromList [0..9]) `shouldBe` Nothing
        describe "qsort" $ do
            it "should sort a vector" $
                qsort (V.fromList [3,4,1,2,5]) `shouldBe` (V.fromList [1,2,3,4,5])

                
