module Prokob.CloneSpec where

import           Control.Applicative
import           Control.Monad
import qualified Data.Array as Array
import qualified Data.Foldable as F
import qualified Data.List as List
import           Input
-- import qualified Data.Vector.Storable as Vector
-- import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified ThunderKV.Static.BinTree as BinTree
import           ThunderKV.Static.Prokob.FromBinTree(layout)
import           ThunderKV.Static.Prokob.Clone
import qualified ThunderKV.Static.Tree as Tree
import           ThunderKV.Static.Tree ( Tree
                                       , FlatNode(..)
                                       , isLeaf
                                       )
import           ThunderKV.Static.Types
import qualified ThunderKV.LargeArray as LargeArray
import Prokob.FromBinTreeSpec( countLeaves
                             , indicesUsed
                             , testH
                             )

spec :: Spec
spec = describe "clone based implementation tests" $ do
    it "size t2" $
      let t2   = templates' 2 Array.! 2
          _foo = link (size 2) (size 1) t2
      in countLeaves t2 `shouldBe`numLeaves 2

    forM_ (Array.assocs $ templates' maxHeight) $ \(h,t) ->
      it ("count leaves from templates' " <> show h) $
        countLeaves t `shouldBe` numLeaves h

    forM_ [0..maxHeight] $ \h ->
      it ("clone: number of leaves of height " <> show h <> " correct") $
        (countLeavesV $ structure h) `shouldBe` numLeaves h

    forM_ [0..maxHeight] $ \h ->
      it ("clone: number of leaves of height " <> show h <> " correct") $
        (countLeavesV $ structure h) `shouldBe` numLeaves h

    prop "indices used clone" $ \(Inputs' h _xs) ->
      let is = indicesUsedV (structure h)
      in [1..(size h)-1] == List.sort is

    forM_ [0..maxHeight] $ \h ->
      it ("clone by height identical " <> show h) $
        structure h `shouldBe` testFH h

    forM_ [0..maxHeight] $ \h ->
      it ("clone diverge by height " <> show h) $
        testDiverge h `shouldBe` Nothing

    prop "clone same as naive" $ \(Inputs' h xs) ->
      let t  = BinTree.fromAscListPow2 xs
          t' = Tree.toTree h . NonEmpty.fromList $ layout t
          tc = structure h
      in divergeAt tc t' == Nothing

    prop "clone same as naive" $ \(Inputs' h xs) ->
      let t   = BinTree.fromAscListPow2 xs
          t'  = Tree.toTree h . NonEmpty.fromList $ layout t
          tc  = fromDescListPow2 h (NonEmpty.reverse xs)
      in t' == tc

    prop "reconstruct" $ \(Inputs' h xs) ->
      let bt = BinTree.fromAscListPow2 xs
          t  = fromDescListPow2 h (NonEmpty.reverse xs)
          t' = Tree.asBinTree t
      in bt == t'

filled      :: Height -> NonEmpty.NonEmpty (Key,Value) -> Tree
filled h xs = Tree.fillDesc (reverse $ F.toList xs) $ structure h

testFH   :: Height -> Tree
testFH h = Tree.toTree h . NonEmpty.fromList . layout $ testH h

testDiverge   :: Height -> Maybe (Index, FlatNode, FlatNode)
testDiverge h = divergeAt (testFH h) (structure h)

divergeAt       :: Tree -> Tree -> Maybe (Index, FlatNode, FlatNode)
divergeAt (Tree.asArray -> t1) (Tree.asArray -> t2) = go 0
  where
    go i = case (t1 LargeArray.! i, t2 LargeArray.! i) of
             (FlatLeaf _ _, FlatLeaf _ _)       -> Nothing
             (u@(FlatNode l1 _ r1), v@(FlatNode l2 _ r2))
               | l1 == l2 && r1 == r2 -> go l1 <|> go r1
               | otherwise            -> Just (i,u,v)
             (u,v)                    -> Just (i,u,v)



-- countLeavesV :: Foldable f => f FlatNode -> Capacity
countLeavesV = List.genericLength . filter isLeaf . LargeArray.toList . Tree.asArray

-- indicesUsedV :: Foldable f => f FlatNode -> [Index]
indicesUsedV = indicesUsed . LargeArray.toList . Tree.asArray
