module Input where


import           Control.Applicative
import           Control.Monad
import qualified Data.Array as Array
import qualified Data.Foldable as F
import qualified Data.List as List
-- import qualified Data.Vector.Storable as Vector
-- import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           ThunderKV.Static.BinTree
import           ThunderKV.Static.Prokob
import qualified ThunderKV.Static.Prokob.Clone       as Clone
import qualified ThunderKV.Static.Prokob.FromBinTree as FromBinTree

import           ThunderKV.Static.Tree
import           ThunderKV.Static.Types
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

instance (Arbitrary a) => Arbitrary (Inputs' a) where
  arbitrary = do h <- chooseBoundedIntegral (0,8)
                 Inputs h <$> genPow2 h arbitrary
  shrink (Inputs h xs) = [ Inputs i (NonEmpty.fromList $ NonEmpty.take (2^i) xs)
                         | i <- upTo h
                         ]
upTo h = case h of
           0 -> []
           _ -> [0..h -1]
