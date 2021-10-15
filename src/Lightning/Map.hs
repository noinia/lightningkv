{-# LANGUAGE ScopedTypeVariables #-}
module Lightning.Map
  ( GMap
  , Map
  , fromAscList
  , insert
  , lookup
  ) where


import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Lightning.Prokob as Prokob
import           Lightning.Tree (GTree, Layout(..))
import           Prelude hiding (lookup)
import qualified Lightning.Prokob as Prokob

--------------------------------------------------------------------------------

newtype Key k = Key Int deriving (Show,Eq,Ord)

----------------------------------------

newtype Index v = Index Int deriving (Show,Eq,Ord)

--------------------------------------------------------------------------------

data GMap f g k v = Map (GTree VEB f (Key k) (Index v))
                        (g v)

-- instance Functor g => Functor (GMap f g k) where
--   fmap f (Map t v) = Map (coerce' f t) (fmap f v)
--     where
--       coerce'   :: (a -> b) -> GTree f (Key k) (Index a) -> GTree f (Key k) (Index b)
--       coerce' _ = coerce


type Map = GMap UV.Vector V.Vector






fromAscList    :: [(k,v)] -> Map k v
fromAscList xs = Map (Prokob.fromAscListN (length xs) $ map fst xs)


insert :: (Ord k, Enum k) => k -> v -> Map k v -> Map k v
insert = undefined

lookup :: (Ord k, Enum k) => k -> Map k v -> Maybe v
lookup = undefined


-- --------------------------------------------------------------------------------

-- fromAscList :: [(k,v)] -> Map k v
-- fromAscList = undefined


-- insert :: (Ord k, Enum k) => k -> v -> Map k v -> Map k v
-- insert = undefined

-- lookup :: (Ord k, Enum k) => k -> Map k v -> Maybe v
-- lookup = undefined
