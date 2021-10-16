{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thunder.Map
  ( Map
  , fromAscList, fromAscListN

  -- , insert
  , lookup

  , Key, mkKey

  , GMap
  ) where


import           Control.Arrow (first)
import           Data.Semigroup
import qualified Data.Vector as V
import Data.Vector.Primitive(Prim(..))
-- -- import qualified Data.Vector.Unboxed as UV
import           Foreign.Storable
import qualified Thunder.Prokob as Prokob
import           Thunder.Tree (GTree, Layout(..), searchLeafR)
import           Prelude hiding (lookup)
-- -- import qualified Data.Vector.Storable.Mutable as UMV

--------------------------------------------------------------------------------

-- | Key representing something of type k
newtype Key k = Key Int deriving (Show,Eq,Ord,Storable,Prim)

-- | Make a Key
mkKey :: Enum k => k -> Key k
mkKey = Key . fromEnum

-- -- instance UV.Unbox (Key k)

-- newtype instance UV.MVector s (Key k) = KMV (UV.MVector s Int)
-- newtype instance UV.Vector  (Key k) = KV (UV.Vector Int)

-- deriving instance UMV.MVe

----------------------------------------

-- | Index that should return s.t. of type v
newtype Index v = Index Int deriving (Show,Eq,Ord,Storable,Prim)

--------------------------------------------------------------------------------

-- | Key values represented by unpacking their indices.
data KeyValue k v = KeyValue { getKey        :: {-# UNPACK #-} !(Key k)
                             , getValueIndex :: {-# UNPACK #-} !(Index v)
                             }
                  deriving (Show)

mkKeyValue     :: Enum k => k -> Int -> KeyValue k v
mkKeyValue k i = KeyValue (mkKey k) (Index i)

instance Eq (KeyValue k v) where
  -- | comparison based on key's only
  (KeyValue k _) == (KeyValue k' _) = k == k'
instance Ord (KeyValue k v) where
  -- | comparison based on key's only
  (KeyValue k _) `compare` (KeyValue k' _) = k `compare` k'

--------------------------------------------------------------------------------

-- | VEB Tree based map.
data GMap f g k v = Map (GTree VEB f (Key k) (KeyValue k v))
                        (g v)
-- FIXME: Map is static for now.

instance (Show k, Show v) => Show (Map k v) where
  show (Map t vs) = show (t, vs)
  -- TODO: FIXME.

-- instance Functor g => Functor (GMap f g k) where
--   fmap f (Map t v) = Map (coerce' f t) (fmap f v)
--     where
--       coerce'   :: (a -> b) -> GTree f (Key k) (Index a) -> GTree f (Key k) (Index b)
--       coerce' _ = coerce


type Map = GMap V.Vector V.Vector

--------------------------------------------------------------------------------

-- | Builds a Map of key value pairs xs.
--
-- pre: length xs == 2^h for some h.
fromAscList    :: Enum k => [(k,v)] -> Map k v
fromAscList xs = fromAscListN (length xs) xs

-- | Given a length n = 2^h and a list of key value pairs xs, builds a Map.
--
-- pre: length xs == 2^h for some h.
fromAscListN      :: forall k v. Enum k => Prokob.Size -> [(k,v)] -> Map k v
fromAscListN n xs = let (ks,vs) = first (zipWith (flip (mkKeyValue @k @v)) [0..]) $ unzip xs
                    in Map (Prokob.fromAscListNWith getKey n ks)
                           (V.fromList vs)


--------------------------------------------------------------------------------
-- * Insertions

insert :: Enum k => k -> v -> Map k v -> Map k v
insert = undefined


--------------------------------------------------------------------------------
-- * Deletions

delete :: Enum k => k -> Map k v -> Map k v
delete = undefined

--------------------------------------------------------------------------------
-- * Queries

-- | Lookup a key in the map
lookup                                     :: Enum k => k -> Map k v -> Maybe v
lookup (mkKey -> q) (Map t vs) | k == q    = Just $ vs V.! i
                               | otherwise = Nothing
  where
    (KeyValue k (Index i)) = searchLeafR (q >) t
