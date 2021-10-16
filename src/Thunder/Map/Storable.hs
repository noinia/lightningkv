module Thunder.Map.Storable
  ( Map
  , fromAscList, fromAscListN
  , toAscList
  -- , insert
  , lookup
  , lookupGE
  ) where

import qualified Data.Vector.Storable as SV
import           Foreign.Storable
import qualified Thunder.Map.Base as Base
import           Thunder.Tree (Size)
import           Prelude hiding (lookup)

--------------------------------------------------------------------------------

type Map = Base.GMap SV.Vector SV.Vector

-- | Produce all key,value pairs in ascending order.
toAscList :: (Enum k, Storable v) => Map k v -> [(k,v)]
toAscList = Base.toAscList

--------------------------------------------------------------------------------

-- | Builds a Map of key value pairs xs.
fromAscList :: (Enum k, Storable v) => [(k,v)] -> Map k v
fromAscList = Base.fromAscList

-- | Given a length n = 2^h and a list of key value pairs xs, builds a Map.
fromAscListN :: (Enum k, Storable v) => Size -> [(k,v)] -> Map k v
fromAscListN = Base.fromAscListN

--------------------------------------------------------------------------------
-- * Insertions

-- insert :: Enum k => k -> v -> Map k v -> Map k v
-- insert = Base.insert


-- --------------------------------------------------------------------------------
-- -- * Deletions

-- delete :: Enum k => k -> Map k v -> Map k v
-- delete = undefined

--------------------------------------------------------------------------------
-- * Queries

-- | Lookup a key in the map
lookup :: ( Enum k, Storable v) => k -> Map k v -> Maybe v
lookup = Base.lookup

-- | Successor query (lookupGE) in the map
lookupGE :: (Enum k, Storable v) => k -> Map k v -> Maybe (k,v)
lookupGE = Base.lookupGE
