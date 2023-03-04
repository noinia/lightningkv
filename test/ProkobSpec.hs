module ProkobSpec
  ( spec
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import           Input
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import qualified ThunderKV.Static.Map as Map
import qualified NaiveMap
-- import           ThunderKV.Static.BinTree
-- import           ThunderKV.Static.Prokob
-- import qualified ThunderKV.Static.Prokob.Clone as Clone
-- import qualified ThunderKV.Static.Prokob.FromBinTree as FromBinTree
-- import           ThunderKV.Static.Tree
import           ThunderKV.Static.Types
-- import qualified ThunderKV.LargeArray as LargeArray

--------------------------------------------------------------------------------

buildNaive   :: Map.Map -> NaiveMap.Map Key Value
buildNaive m = NaiveMap.fromDescListPow2 (Map.heightOf m) (Map.toDescList m)

spec :: Spec
spec = describe "Map tests" $ do
         prop "lookupGE tests same as naive" $ \q m ->
           Map.lookupGE q m == NaiveMap.lookupGE q (buildNaive m)


         -- Key 0

mapX :: Map.Map
mapX = read "Map 2 (Tree (LargeArray [FlatNode 1 (Key 2) 2,FlatNode 3 (Key (-1)) 4,FlatNode 5 (Key 2) 6,FlatLeaf (Key (-1)) (Value 0),FlatLeaf (Key 2) (Value 2),FlatLeaf (Key 2) (Value (-1)),FlatLeaf (Key (-2)) (Value 2)]))"
