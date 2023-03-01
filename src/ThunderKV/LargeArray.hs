module ThunderKV.LargeArray
  ( LargeArray
  , fromListN
  , (!)
  , toList
  ) where


import           Control.DeepSeq
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import           Foreign.Storable
import           GHC.Generics (Generic)
import           ThunderKV.Static.Types

--------------------------------------------------------------------------------

newtype LargeArray a = LargeArray (Vector a)
  deriving stock (Show,Read,Eq,Ord,Generic)

instance NFData a => NFData (LargeArray a)

-- | get the underlying vector
toVector                :: LargeArray a -> Vector a
toVector (LargeArray v) = v

-- | Access element i
(!)                :: Storable a => LargeArray a -> Index -> a
(LargeArray v) ! i = v Vector.! i

-- | Construct a large array from a list of elements.
fromListN   :: Storable a => Size -> [a] -> LargeArray a
fromListN n = LargeArray . Vector.fromListN n

toList :: Storable a => LargeArray a -> [a]
toList = Vector.toList . toVector
