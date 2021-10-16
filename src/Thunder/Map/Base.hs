{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thunder.Map.Base
  ( GMap(..)
  , fromAscList, fromAscListN
  , toAscList, toAscVector

  -- , insert
  , lookup
  , lookupGE
  ) where


import           Control.Arrow (first)
import           Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as SV
import           GHC.Generics
-- -- import qualified Data.Vector.Unboxed as UV
import qualified Thunder.Prokob as Prokob
import           Thunder.Tree
import           Thunder.KeyValue
import           Thunder.WithInfty
import           Prelude hiding (lookup)
-- -- import qualified Data.Vector.Storable.Mutable as UMV

--------------------------------------------------------------------------------

-- | VEB Tree based map.
data GMap f g k v = Map (GTree VEB f (WithInfty (Key k)) (WithInfty (KeyValue k v)))
                        (g v)
                    deriving (Generic)
-- FIXME: Map is static for now.

instance (Show k, Enum k, Show v
         , GV.Vector f (Node (WithInfty (Key k)) (WithInfty (KeyValue k v)))
         , GV.Vector f (WithInfty (KeyValue k v))
         , GV.Vector g v
         ) => Show (GMap f g k v) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toAscList m)

instance ( NFData (f (Node (WithInfty (Key k)) (WithInfty (KeyValue k v))))
         , NFData (g v)
         ) => NFData (GMap f g k v)

-- instance Functor g => Functor (GMap f g k) where
--   fmap f (Map t v) = Map (coerce' f t) (fmap f v)
--     where
--       coerce'   :: (a -> b) -> GTree f (Key k) (Index a) -> GTree f (Key k) (Index b)
--       coerce' _ = coerce



-- | Produce all key,value pairs in ascending order.
toAscVector            :: ( GV.Vector w (k,v), GV.Vector w k, GV.Vector w v
                          , GV.Vector f (Node (WithInfty (Key k)) (WithInfty (KeyValue k v)))
                          , GV.Vector f (WithInfty (KeyValue k v))
                          , GV.Vector w (WithInfty (KeyValue k v))
                          , GV.Vector g v
                          , Enum k
                          )
                       => GMap f g k v -> w (k,v)
toAscVector (Map t vs) = GV.zip ks (GV.convert vs)
  where
    ks = GV.mapMaybe (fmap (unKey . getKey) . toMaybe) . GV.convert $ leaves t

-- | Produce all key,value pairs in ascending order.
toAscList :: ( GV.Vector f (Node (WithInfty (Key k)) (WithInfty (KeyValue k v)))
             , GV.Vector f (WithInfty (KeyValue k v))
             , GV.Vector g v
             , Enum k
             )
          => GMap f g k v -> [(k,v)]
toAscList = V.toList . toAscVector

--------------------------------------------------------------------------------

-- | Builds a Map of key value pairs xs.
fromAscList    :: (Enum k, GV.Vector g v) => [(k,v)] -> GMap SV.Vector g k v
fromAscList xs = fromAscListN (length xs) xs

-- | Given a length n and a list of key value pairs xs, builds a Map.
fromAscListN      :: (Enum k, GV.Vector g v) => Size -> [(k,v)] -> GMap SV.Vector g k v
fromAscListN n xs = let (ks,vs) = first (zipWith (flip mkKeyValue) [0..]) $ unzip xs
                    in Map (Prokob.fromAscListNWith getKey n ks)
                           (GV.fromList vs)



--------------------------------------------------------------------------------
-- * Insertions

insert :: Enum k => k -> v -> GMap f g k v -> GMap f g k v
insert = undefined


--------------------------------------------------------------------------------
-- * Deletions

delete :: Enum k => k -> GMap f g k v -> GMap f g k v
delete = undefined

--------------------------------------------------------------------------------
-- * Queries

-- | Lookup a key in the map
lookup                         :: ( Enum k
                                  , GV.Vector g v
                                  , GV.Vector f (Node (WithInfty (Key k))
                                                      (WithInfty (KeyValue k v)))
                                  )
                               => k -> GMap f g k v -> Maybe v
lookup (mkKey -> q) (Map t vs) = case searchLeafR (Val q > ) t of
    Val (KeyValue k (Index i)) | q == k   -> Just $ vs GV.! i
                               |otherwise -> Nothing
    Infty                                 -> Nothing

-- | Successor query (lookupGE) in the map
lookupGE                         :: ( Enum k
                                    , GV.Vector g v
                                    , GV.Vector f (Node (WithInfty (Key k))
                                                        (WithInfty (KeyValue k v)))
                                    ) => k -> GMap f g k v -> Maybe (k,v)
lookupGE (mkKey -> q) (Map t vs) = case searchLeafR (Val q > ) t of
    Val (KeyValue k (Index i)) | q <= k   -> Just $ (unKey k, vs GV.! i)
                               |otherwise -> Nothing
    Infty                                 -> Nothing
