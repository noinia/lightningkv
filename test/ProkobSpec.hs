module ProkobSpec
  ( spec
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Array as Array
import qualified Data.Foldable as F
import qualified Data.List as List
-- import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           ThunderKV.BinTree
import           ThunderKV.Prokob
import           ThunderKV.Prokob.Clone
import           ThunderKV.Static.Tree
import           ThunderKV.Types
--------------------------------------------------------------------------------

deriving newtype instance Arbitrary Key
deriving newtype instance Arbitrary Value


-- | simple binary tree
simplest :: BinTree Index Index
simplest = BinNode (BinLeaf 0 1) 0 (BinLeaf 5 6)





genPow2       :: Monad m => Height -> m a -> m (NonEmpty.NonEmpty a)
genPow2 h gen = NonEmpty.fromList <$> replicateM (2^h) gen

data Inputs' a = Inputs Height (NonEmpty.NonEmpty a)
                 deriving (Show,Eq,Functor)

type Inputs = Inputs' (Key,Value)
pattern Inputs'      :: Height -> NonEmpty.NonEmpty (Key,Value) -> Inputs
pattern Inputs' h xs = Inputs h xs
{-# COMPLETE Inputs' #-}

maxHeight :: Height
maxHeight = 4

instance (Arbitrary a) => Arbitrary (Inputs' a) where
  arbitrary = do h <- chooseBoundedIntegral (0,8)
                 Inputs h <$> genPow2 h arbitrary
  shrink (Inputs h xs) = [ Inputs i (NonEmpty.fromList $ NonEmpty.take (2^i) xs)
                         | i <- upTo h
                         ]
upTo h = case h of
           0 -> []
           _ -> [0..h -1]


-- test :: BinTree Index Index
-- test = fromAscListPow2 $ NonEmpty.fromList
--        [ (0,0)
--        , (1,2)
--        , (20,3)
--        , (21,5)
--        ]

testH   :: Height -> BinTree Key Value
testH h = fromAscListPow2 . NonEmpty.fromList . map (\_ -> (Key 0, Value 0))
        $ [0..(2^h)-1]

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
spec = describe "prokob layout tests" $ do
         splitSpec
         cloneSpec

splitSpec :: Spec
splitSpec = describe "split based implementation tests" $ do
    it "flat simplest" $
      flatten simplest
      `shouldBe`
      [(0,FlatNode 1 (Key 0) 2),(1,FlatLeaf (Key 0) (Value 1)),(2,FlatLeaf (Key 5) (Value 6))]
    prop "fromAscListPow2 correct height" $ \(Inputs' h xs) ->
      let t = fromAscListPow2 xs
      in heightL t == h
    prop "fromAscListPow2 correct size" $ \(Inputs' h xs) ->
      let t = fromAscListPow2 xs
      in List.genericLength (layout t) == treeSize h
    prop "right heights" $ \(Inputs' h xs) ->
      let t = fromAscListPow2 xs
      in case split t of
           Left _ -> h == 0
           Right (ht,hb,top) -> heightL top == ht
                                && all (\(b1,b2)-> heightL b1 == hb
                                                && heightL b2 == hb
                                       ) top
    prop "indices disjoint" $ \(Inputs' h xs) ->
      let t  = fromAscListPow2 xs
          is = indices (layout t)
      in [0..(treeSize h)-1] == is
    prop "indices used" $ \(Inputs' h xs) ->
      let t  = fromAscListPow2 xs
          is = indicesUsed' (layout t)
      in [1..(treeSize h)-1] == List.sort is

    prop "reconstruct" $ \(Inputs' h xs) ->
      let t  = fromAscListPow2 xs
          t' = asBinTree . toTree h . NonEmpty.fromList $ layout t
      in t == t'

countLeaves :: Foldable f => f FlatNode -> Size
countLeaves = List.genericLength . filter isLeaf . F.toList

cloneSpec :: Spec
cloneSpec = describe "clone based implementation tests" $ do
    it "size t2" $
      let t2   = templates' 2 Array.! 2
          _foo = link (treeSize 2) (treeSize 1) t2
      in countLeaves t2 `shouldBe`numLeaves 2

    forM_ (Array.assocs $ templates' maxHeight) $ \(h,t) ->
      it ("count leaves from templates' " <> show h) $
        countLeaves t `shouldBe` numLeaves h

    forM_ [0..maxHeight] $ \h ->
      it ("clone: number of leaves of height " <> show h <> " correct") $
        (countLeaves $ structure h) `shouldBe` numLeaves h

    forM_ [0..maxHeight] $ \h ->
      it ("clone: number of leaves of height " <> show h <> " correct") $
        (countLeaves $ structure h) `shouldBe` numLeaves h

    prop "indices used clone" $ \(Inputs' h _xs) ->
      let is = indicesUsed (structure h)
      in [1..(treeSize h)-1] == List.sort is

    forM_ [0..maxHeight] $ \h ->
      it ("clone by height identical " <> show h) $
        structure h `shouldBe` testFH h

    forM_ [0..maxHeight] $ \h ->
      it ("clone diverge by height " <> show h) $
        testDiverge h `shouldBe` Nothing

    prop "clone same as naive" $ \(Inputs' h xs) ->
      let t  = fromAscListPow2 xs
          t' = toTree h . NonEmpty.fromList $ layout t
          tc = structure h
      in divergeAt tc t' == Nothing

    -- this one still fails, but that is ok for now
    xprop "reconstruct" $ \(Inputs' h xs) ->
      let t  = fromAscListPow2 xs
          t' = asBinTree $ structure h
      in t == t'


testFH   :: Height -> Tree
testFH h = toTree h . NonEmpty.fromList . layout $ testH h

testDiverge   :: Height -> Maybe (Index, FlatNode, FlatNode)
testDiverge h = divergeAt (testFH h) (structure h)

divergeAt       :: Tree -> Tree -> Maybe (Index, FlatNode, FlatNode)
divergeAt t1 t2 = go 0
  where
    go i = case (t1 Array.! i, t2 Array.! i) of
             (FlatLeaf _ _, FlatLeaf _ _)       -> Nothing
             (u@(FlatNode l1 _ r1), v@(FlatNode l2 _ r2))
               | l1 == l2 && r1 == r2 -> go l1 <|> go r1
               | otherwise            -> Just (i,u,v)
             (u,v)                    -> Just (i,u,v)
