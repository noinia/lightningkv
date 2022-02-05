{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.KeyValue
  ( Key, mkKey, unKey
  , Index(..)
  , KeyValue(..), mkKeyValue
  ) where

import Control.DeepSeq
import Data.Vector.Primitive (Prim)
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import Prelude hiding (lookup)

import Thunder.Node(Node, ConstructNode(..), DestructNode(..))
import qualified Thunder.Node as Node

--------------------------------------------------------------------------------

-- | Key representing something of type k
newtype Key k = Key Node.Key
              deriving (Show,Eq,Ord,Storable,Prim,NFData) via Node.Key

-- | Make a Key
mkKey :: Enum k => k -> Key k
mkKey = Key . fromEnum

unKey         :: Enum k => Key k -> k
unKey (Key k) = toEnum k

--------------------------------------------------------------------------------

-- | Index that should return s.t. of type v
newtype Index v = Index Node.Value deriving (Show,Eq,Ord,Storable,Prim,NFData) via Node.Value

--------------------------------------------------------------------------------

-- | Key values represented by unpacking their indices.
data KeyValue k v = KeyValue { getKey        :: {-# UNPACK #-} !(Key k)
                             , getValueIndex :: {-# UNPACK #-} !(Index v)
                             }
                  deriving stock (Show,Generic,Generic1)
                  deriving anyclass (GStorable)

mkKeyValue     :: Enum k => k -> Int -> KeyValue k v
mkKeyValue k i = KeyValue (mkKey k) (Index i)

instance NFData (KeyValue k v)
-- instance NFData1 (KeyValue k)

instance Eq (KeyValue k v) where
  -- | comparison based on key's only
  (KeyValue k _) == (KeyValue k' _) = k == k'
instance Ord (KeyValue k v) where
  -- | comparison based on key's only
  (KeyValue k _) `compare` (KeyValue k' _) = k `compare` k'


--------------------------------------------------------------------------------

data instance Node (Key k) (KeyValue k v) = LeafKV {-# UNPACK #-} !Node.Key
                                                   {-# UNPACK #-} !Node.Value
                                          | NodeKV {-# UNPACK #-} !Node.Index
                                                   {-# UNPACK #-} !Node.Key
                                                   {-# UNPACK #-} !Node.Index
                                     deriving stock (Show,Eq,Ord,Generic)
                                     deriving anyclass (GStorable)
instance NFData (Node (Key k) (KeyValue k v))

instance ConstructNode (Key k) (KeyValue k v) where
  leaf (KeyValue (Key k) (Index v)) = LeafKV k v
  node l (Key k) r = NodeKV l k r
instance DestructNode (Key k) (KeyValue k v) where
  destructNode n f g = case n of
                         LeafKV k v   -> f $ KeyValue (Key k) (Index v)
                         NodeKV l k r -> g l (Key k) r
