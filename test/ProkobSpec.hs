module ProkobSpec
  ( spec
  ) where

import           Control.Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
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
pattern Inputs'      :: Height -> (NonEmpty.NonEmpty (Key,Value)) -> Inputs
pattern Inputs' h xs = Inputs h xs
{-# COMPLETE Inputs' #-}

instance (Arbitrary a) => Arbitrary (Inputs' a) where
  arbitrary = do h <- chooseBoundedIntegral (0,8)
                 Inputs h <$> genPow2 h arbitrary
  shrink (Inputs h xs) = [ Inputs i (NonEmpty.fromList $ NonEmpty.take (2^i) xs)
                         | i <- [0..(h-1)]
                         ]


test :: BinTree Index Index
test = fromAscListPow2 $ NonEmpty.fromList
       [ (0,0)
       , (1,2)
       , (20,3)
       , (21,5)
       ]

-- test2 = fromAscListPow2 . NonEmpty.fromList . map (\i -> (Key i, Value i))
--       $ [0..(2^3)-1]


flatSimplest :: [(Index,FlatNode)]
flatSimplest = layoutWith Key Value simplest

--------------------------------------------------------------------------------


spec :: Spec
spec = describe "prokob layout tests" $ do
         it "flat simplest" $
           flatSimplest
           `shouldBe`
           [(0,FlatNode 1 (Key 0) 2),(1,FlatLeaf (Key 0) (Value 1)),(2,FlatLeaf (Key 5) (Value 6))]
         prop "fromAscListPow2 correct size/height" $ \(Inputs' h xs) ->
           let t = fromAscListPow2 xs
           in heightL t == h
         prop "right heights" $ \(Inputs' h xs) ->
           let t = fromAscListPow2 xs
           in case split t of
                Left _ -> h == 0
                Right (ht,hb,top) -> heightL top == ht
                                     && all (\(b1,b2)-> heightL b1 == hb
                                                     && heightL b2 == hb
                                            ) top
