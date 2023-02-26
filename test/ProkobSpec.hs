module ProkobSpec
  ( spec
  ) where

import           Control.Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           ThunderKV.BinTree
import           ThunderKV.Prokob
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

instance (Arbitrary a) => Arbitrary (Inputs' a) where
  arbitrary = do h <- chooseBoundedIntegral (0,8)
                 Inputs h <$> genPow2 h arbitrary
  shrink (Inputs h xs) = [ Inputs i (NonEmpty.fromList $ NonEmpty.take (2^i) xs)
                         | i <- upTo h
                         ]
upTo h = case h of
           0 -> []
           h -> [0..h -1]


test :: BinTree Index Index
test = fromAscListPow2 $ NonEmpty.fromList
       [ (0,0)
       , (1,2)
       , (20,3)
       , (21,5)
       ]

testH   :: Height -> BinTree Key Value
testH h = fromAscListPow2 . NonEmpty.fromList . map (\i -> (Key i, Value i))
        $ [0..(2^h)-1]

testx :: BinTree Key Value
testx = fromAscListPow2 $ (Key 0,Value 1) :| [(Key 1,Value 1)]


flatten :: BinTree Index Index -> [(Index,FlatNode)]
flatten = layoutWith Key Value

indices :: [(Index,a)] -> [Index]
indices = map fst

indicesUsed :: [(a,FlatNode)] -> [Index]
indicesUsed = concatMap (\(_,n) -> case n of
                                  FlatLeaf _ _ -> []
                                  FlatNode l _ r -> [l,r]
                        )

--------------------------------------------------------------------------------


spec :: Spec
spec = describe "prokob layout tests" $ do
         it "flat simplest" $
           flatten simplest
           `shouldBe`
           [(0,FlatNode 1 (Key 0) 2),(1,FlatLeaf (Key 0) (Value 1)),(2,FlatLeaf (Key 5) (Value 6))]
         prop "fromAscListPow2 correct height" $ \(Inputs' h xs) ->
           let t = fromAscListPow2 xs
           in heightL t == h
         prop "fromAscListPow2 correct size" $ \(Inputs' h xs) ->
           let t = fromAscListPow2 xs
           in List.genericLength (layout t) == size h
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
           in [0..(size h)-1] == is
         prop "indices used" $ \(Inputs' h xs) ->
           let t  = fromAscListPow2 xs
               is = indicesUsed (layout t)
           in [1..(size h)-1] == List.sort is
         prop "reconstruct" $ \(Inputs' h xs) ->
           let t  = fromAscListPow2 xs
               t' = asBinTree . toTree h $ layout t
           in t == t'
