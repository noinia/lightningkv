module ThunderKV.Types
  ( Index
  , Height
  , Key(..)
  , Value(..)
  , size
  , Size

  , Version(..)

  , LeafValue(..)
  , RoutingKey(..)
  ) where

import           Control.DeepSeq
import           Data.Word
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

type Index = Word64

-- | Measures the height of the tree, leaves have height 0
type Height = Word64

newtype Key = Key Index
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,Enum,NFData)

newtype Value = Value Index
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,Enum,NFData)

-- | Size of a tree of height h
size   :: Height -> Size
size h = 2 ^ (h + 1) - 1

type Size = Index

newtype Version = Version Word64
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,Enum,NFData)


data LeafValue = Empty
               | DeletedAt {-# UNPACK #-}!Version
                           {-# UNPACK #-}!Key
                           {-# UNPACK #-}!Value
               | JustKV    {-# UNPACK #-}!Key
                           {-# UNPACK #-}!Value
               deriving stock (Show,Read,Eq,Ord,Generic)
instance NFData LeafValue


data RoutingKey = JustKey   {-# UNPACK #-}!Key
                | Infty
                deriving stock (Show,Read,Eq,Ord,Generic)

instance NFData RoutingKey
