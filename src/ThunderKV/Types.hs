module ThunderKV.Types
  ( Index
  , Height
  , Key(..)
  , Value(..)
  , treeSize
  , numLeaves
  , Size

  , Version(..)

  , LeafValue(..)
  , RoutingKey(..)
  ) where

import Control.DeepSeq
import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Indices that we use are just Word's
type Index = Word64

-- | Measures the height of the tree, leaves have height 0
type Height = Word64

-- | The Size of various things (trees, arrays etc.)
type Size = Index
  -- be *very* careful when changing this, since we treat these really
  -- as the same type.



newtype Key = Key Index
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,NFData,Storable)

newtype Value = Value Index
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,NFData,Storable)



-- | Size of a tree of height h
treeSize   :: Height -> Size
treeSize h = 2 ^ (h + 1) - 1

-- | Number of leaves (and therefore number of elements) in a tree of
-- size h: 2 ^ h.
numLeaves   :: Height -> Size
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
