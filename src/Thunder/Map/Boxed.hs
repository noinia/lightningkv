module Thunder.Map.Boxed
  ( Map
  , fromAscList, fromAscListN
  , toAscList
  -- , insert
  , lookup
  , lookupGE
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import           Prelude hiding (lookup)
import qualified Thunder.Map.Base as Base
import           Thunder.Tree (Size)

--------------------------------------------------------------------------------

type Map = Base.Map SV.Vector V.Vector

-- | Produce all key,value pairs in ascending order.
toAscList :: (Enum k) => Map k v -> [(k,v)]
toAscList = Base.toAscList

--------------------------------------------------------------------------------

-- | Builds a Map of key value pairs xs.
fromAscList :: (Enum k) => [(k,v)] -> Map k v
fromAscList = Base.fromAscList

-- | Given a length n = 2^h and a list of key value pairs xs, builds a Map.
fromAscListN :: (Enum k) => Size -> [(k,v)] -> Map k v
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
lookup :: (Enum k) => k -> Map k v -> Maybe v
lookup = Base.lookup

-- | Successor query (lookupGE) in the map
lookupGE :: (Enum k) => k -> Map k v -> Maybe (k,v)
lookupGE = Base.lookupGE
