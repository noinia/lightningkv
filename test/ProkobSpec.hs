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

data Inputs = Inputs Height (NonEmpty.NonEmpty (Key,Value))
               deriving (Show,Eq)

instance Arbitrary Inputs where
  arbitrary = do h <- chooseBoundedIntegral (0,8)
                 Inputs h <$> genPow2 h arbitrary

-- test :: BinTree Index Index
-- test = fromAscListPow2 $ NonEmpty.fromList
--        [ (0,0)
--        , (1,2)
--        , (20,3)
--        , (21,5)
--        ]

-- test2 = fromAscListPow2 . NonEmpty.fromList . map (\i -> (Key i, Value i))
--       $ [0..(2^3)-1]


-- flatSimplest = layoutWith Key Value simplest

--------------------------------------------------------------------------------


spec :: Spec
spec = describe "prokob layout tests" $
         prop "fromAscListPow2 correct size/height" $ \(Inputs h xs) ->
           let t = fromAscListPow2 xs
           in heightL t == h
