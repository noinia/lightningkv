module ThunderKV.Types
  ( Index
  , Height
  , Key(..)
  , Value(..)
  , size
  , Size

  , Version(..)

  , MaybeValue(..)
  , KeyOrDeleted(..)
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




data MaybeValue = NoValue
                | JustValue {-# UNPACK #-} !Value
                deriving stock (Show,Eq,Ord,Generic)

instance NFData MaybeValue


data KeyOrDeleted = DeletedAt {-# UNPACK #-}!Version
                              {-# UNPACK #-}!Key
                  | StillHere {-# UNPACK #-}!Key
                  deriving stock (Show,Eq,Ord,Generic)

instance NFData KeyOrDeleted


data RoutingKey = JustKey   {-# UNPACK #-}!Key
                | Infty
                deriving stock (Show,Eq,Ord,Generic)

instance NFData RoutingKey
