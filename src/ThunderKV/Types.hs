module ThunderKV.Types
  ( Index
  , Height
  , Key(..)
  , Value(..)
  , size
  , numLeaves
  , Size
  , Capacity
  , asSize

  , Version(..)

  , LeafValue(..)
  , RoutingKey(..)
  ) where

import Control.DeepSeq
import Data.Ix
import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Indices that we use
type Index = Int

-- | The Size of the Tree
type Size = Index

-- | Measures the height of the tree, leaves have height 0
newtype Height = Height Word8
  deriving newtype (Show,Read,Eq,Ord,Num,Real,Enum,Bounded,Integral,Ix)


-- | Models the number of elements that we can store in our trees.
newtype Capacity = Capacity Index
  deriving newtype (Show,Read,Eq,Ord,Num,Real,Enum,Integral,Ix)

-- | Interpet a capacity as a size.
asSize              :: Capacity -> Size
asSize (Capacity i) = i

instance Bounded Capacity where
  minBound = 0
  maxBound = Capacity $ maxBound @Index `div` 2
  -- since a tree of n elements has size 2n-1

newtype Key = Key Index
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,NFData,Storable)

newtype Value = Value Index
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,NFData,Storable)

-- | Size of a tree of height h
size   :: Height -> Size
size h = 2 ^ (h + 1) - 1

-- | Number of leaves (and therefore number of elements) in a tree of
-- size h: 2 ^ h.
numLeaves   :: Height -> Capacity
numLeaves h = 2 ^ h


newtype Version = Version Word64
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,NFData,Storable)


-- FIXME: internal nodes also need to know when they are empty
-- so just implement empty structures at: deletedAsOf (time 0)
-- create a separate constructor in nodes for this I think.

data LeafValue = Empty
               | DeletedAt {-# UNPACK #-}!Version
                           {-# UNPACK #-}!Key
                           {-# UNPACK #-}!Value
               | JustKV    {-# UNPACK #-}!Key
                           {-# UNPACK #-}!Value
               deriving stock (Show,Read,Eq,Ord,Generic)


instance NFData LeafValue

-- | these should not be needed
data RoutingKey = JustKey   {-# UNPACK #-}!Key
                | Infty
                deriving stock (Show,Read,Eq,Ord,Generic)

instance NFData RoutingKey
