module Input where

import           Control.Monad
-- import qualified Data.Vector.Storable as Vector
-- import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Test.QuickCheck
-- import           ThunderKV.Static.BinTree
-- import           ThunderKV.Static.Prokob
-- import           ThunderKV.Static.Tree
import           Foreign.Storable
import           ThunderKV.Static.Types
import qualified ThunderKV.Static.Map as Map
import qualified ThunderKV.LargeArray as LargeArray

--------------------------------------------------------------------------------

deriving newtype instance Arbitrary Key
deriving newtype instance Arbitrary Value

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

instance Arbitrary Inputs where
  -- generates striclty positive keys
  arbitrary = do h <- chooseBoundedIntegral (0,8)
                 Inputs h <$> (genPow2 h $ arbitrary `suchThat` (\(k,_) -> k > Key 0))
  shrink (Inputs h xs) = [ Inputs i (NonEmpty.fromList $ NonEmpty.take (2^i) xs)
                         | i <- upTo h
                         ]
upTo   :: Height -> [Height]
upTo h = case h of
           0 -> []
           _ -> [0..h -1]

fromInputs :: Inputs -> Map.Map
fromInputs (Inputs h xs) = Map.fromDescListPow2 h (NonEmpty.reverse xs)

toInputs   :: Map.Map -> Inputs
toInputs m = Inputs (Map.heightOf m) (Map.toAscList m)

-- | Make sure the keys are unique; since all keys are strictly positive we just keep summing them.
asUniqueKeys               :: Inputs -> Inputs
asUniqueKeys (Inputs h xs) =
  Inputs h $ NonEmpty.scanl1 (\(Key acc,_) (Key k,v) -> (Key $ acc+k,v)) xs


instance (Arbitrary a, Storable a) => Arbitrary (LargeArray.LargeArray a) where
  arbitrary = do n <- arbitrary `suchThat` (>= 0)
                 LargeArray.fromListN n <$> arbitrary

instance Arbitrary Map.Map where
  arbitrary = fromInputs . asUniqueKeys <$> arbitrary
  shrink = map fromInputs . shrink . toInputs
