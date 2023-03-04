module Prokob.FromBinTreeSpec where

import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Input
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           ThunderKV.Static.BinTree ( BinTree(..)
                                          , heightL
                                          )
import qualified ThunderKV.Static.BinTree as BinTree
import           ThunderKV.Static.Prokob.FromBinTree
import           ThunderKV.Static.Tree ( FlatNode(..)
                                       , isLeaf
                                       )
import qualified ThunderKV.Static.Tree as Tree
import           ThunderKV.Static.Types

--------------------------------------------------------------------------------

-- | simple binary tree
simplest :: BinTree Index Index
simplest = BinNode (BinLeaf 0 1) 0 (BinLeaf 5 6)

-- test :: BinTree Index Index
-- test = fromAscListPow2 $ NonEmpty.fromList
--        [ (0,0)
--        , (1,2)
--        , (20,3)
--        , (21,5)
--        ]

testH   :: Height -> BinTree Key Value
testH h = BinTree.fromAscListPow2 . NonEmpty.fromList . map (\_ -> (Key 0, Value 0))
        $ [0..numLeaves h - 1]

-- testx :: BinTree Key Value
-- testx = fromAscListPow2 $ (Key 0,Value 1) :| [(Key 1,Value 1)]


flatten :: BinTree Index Index -> [(Index,FlatNode)]
flatten = layoutWith Key Value

indices :: [(Index,a)] -> [Index]
indices = map fst

indicesUsed' :: [(a,FlatNode)] -> [Index]
indicesUsed' = indicesUsed . map snd

indicesUsed :: Foldable f => f FlatNode -> [Index]
indicesUsed = concatMap (\case
                            FlatLeaf _ _ -> []
                            FlatNode l _ r -> [l,r]
                         )

--------------------------------------------------------------------------------



spec :: Spec
spec = describe "split based implementation tests" $ do
    it "flat simplest" $
      flatten simplest
      `shouldBe`
      [(0,FlatNode 1 (Key 0) 2),(1,FlatLeaf (Key 0) (Value 1)),(2,FlatLeaf (Key 5) (Value 6))]
    prop "fromAscListPow2 correct height" $ \(Inputs' h xs) ->
      let t = BinTree.fromAscListPow2 xs
      in heightL t == h
    prop "fromAscListPow2 correct size" $ \(Inputs' h xs) ->
      let t = BinTree.fromAscListPow2 xs
      in List.genericLength (layout t) == size h
    prop "right heights" $ \(Inputs' h xs) ->
      let t = BinTree.fromAscListPow2 xs
      in case split t of
           Left _ -> h == 0
           Right (ht,hb,top) -> heightL top == ht
                                && all (\(b1,b2)-> heightL b1 == hb
                                                && heightL b2 == hb
                                       ) top
    prop "indices disjoint" $ \(Inputs' h xs) ->
      let t  = BinTree.fromAscListPow2 xs
          is = indices (layout t)
      in [0..(size h)-1] == is
    prop "indices used" $ \(Inputs' h xs) ->
      let t  = BinTree.fromAscListPow2 xs
          is = indicesUsed' (layout t)
      in [1..(size h)-1] == List.sort is

    prop "reconstruct" $ \(Inputs' h xs) ->
      let t  = BinTree.fromAscListPow2 xs
          t' = Tree.asBinTree . Tree.toTree h . NonEmpty.fromList $ layout t
      in t == t'

-- instance {-# OVERLAPPING #-} Foldable Vector.Vector where
--   foldr = Vector.foldr

countLeaves :: Foldable f => f FlatNode -> Capacity
countLeaves = List.genericLength . filter isLeaf . F.toList
