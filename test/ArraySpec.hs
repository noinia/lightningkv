module ArraySpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import qualified ThunderKV.LargeArray as LargeArray

spec :: Spec
spec = describe "LargeArray tests" $ do
         it "mapR" $ do
           let f = (+5) :: Int -> Int
               xs = [1..5]
               vs = LargeArray.toList
                  . LargeArray.mapR f
                  . LargeArray.fromListN (length xs) $ xs
           vs `shouldBe` map f xs
