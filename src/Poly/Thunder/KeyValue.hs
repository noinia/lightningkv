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

--------------------------------------------------------------------------------

-- | Key representing something of type k
newtype Key k = Key Int
              deriving (Show,Eq,Ord,Storable,Prim,NFData) via Int


-- | Make a Key
mkKey :: Enum k => k -> Key k
mkKey = Key . fromEnum

unKey         :: Enum k => Key k -> k
unKey (Key k) = toEnum k


-- -- instance UV.Unbox (Key k)

-- newtype instance UV.MVector s (Key k) = KMV (UV.MVector s Int)
-- newtype instance UV.Vector  (Key k) = KV (UV.Vector Int)

-- deriving instance UMV.MVe

--------------------------------------------------------------------------------

-- | Index that should return s.t. of type v
newtype Index v = Index Int deriving (Show,Eq,Ord,Storable,Prim,NFData) via Int

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
