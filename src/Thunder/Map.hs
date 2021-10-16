{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thunder.Map
  ( Map
  , fromAscList, fromAscListN
  , toAscList, toAscVector

  -- , insert
  , lookup


  , GMap
  ) where


import           Control.Arrow (first)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Generic as GV
-- -- import qualified Data.Vector.Unboxed as UV
import qualified Thunder.Prokob as Prokob
import           Thunder.Tree (GTree, Layout(..), searchLeafR, Node, leaves)
import           Thunder.KeyValue
import           Prelude hiding (lookup)
-- -- import qualified Data.Vector.Storable.Mutable as UMV

--------------------------------------------------------------------------------

-- | VEB Tree based map.
data GMap f g k v = Map (GTree VEB f (Key k) (KeyValue k v))
                        (g v)
-- FIXME: Map is static for now.

instance (Show k, Enum k, Show v
         , GV.Vector f (Node (Key k) (KeyValue k v))
         , GV.Vector f (KeyValue k v)
         , GV.Vector g v
         ) => Show (GMap f g k v) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toAscList m)

-- instance Functor g => Functor (GMap f g k) where
--   fmap f (Map t v) = Map (coerce' f t) (fmap f v)
--     where
--       coerce'   :: (a -> b) -> GTree f (Key k) (Index a) -> GTree f (Key k) (Index b)
--       coerce' _ = coerce


type Map = GMap SV.Vector V.Vector


-- | Produce all key,value pairs in ascending order.
toAscVector            :: ( GV.Vector w (k,v), GV.Vector w k, GV.Vector w v
                          , GV.Vector f (Node (Key k) (KeyValue k v))
                          , GV.Vector f (KeyValue k v)
                          , GV.Vector w (KeyValue k v)
                          , GV.Vector g v
                          , Enum k
                          )
                       => GMap f g k v -> w (k,v)
toAscVector (Map t vs) = GV.zip (GV.map (unKey . getKey) . GV.convert $ leaves t) (GV.convert vs)

-- | Produce all key,value pairs in ascending order.
toAscList :: ( GV.Vector f (Node (Key k) (KeyValue k v))
             , GV.Vector f (KeyValue k v)
             , GV.Vector g v
             , Enum k
             )
          => GMap f g k v -> [(k,v)]
toAscList = V.toList . toAscVector

--------------------------------------------------------------------------------

-- | Builds a Map of key value pairs xs.
--
-- pre: length xs == 2^h for some h.
fromAscList    :: Enum k => [(k,v)] -> Map k v
fromAscList xs = fromAscListN (length xs) xs

-- | Given a length n = 2^h and a list of key value pairs xs, builds a Map.
--
-- pre: length xs == 2^h for some h.
fromAscListN      :: Enum k => Prokob.Size -> [(k,v)] -> Map k v
fromAscListN n xs = let (ks,vs) = first (zipWith (flip mkKeyValue) [0..]) $ unzip xs
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
