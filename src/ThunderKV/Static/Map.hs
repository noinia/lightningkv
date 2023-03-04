module ThunderKV.Static.Map
  ( Map
  , heightOf
  , fromDescListPow2
  , toAscList, toDescList
  , lookup
  , lookupGE
  , lookupLE
  ) where

import           Control.DeepSeq
-- import           Control.Monad.Writer
import           Data.Array (Array)
import qualified Data.Array as Array
import           ThunderKV.Static.Types
import qualified ThunderKV.Static.Prokob as Tree
import           ThunderKV.Static.Prokob (Tree
                                         )
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Word
import           GHC.Generics (Generic)
import           Prelude hiding (lookup)

--------------------------------------------------------------------------------

-- | The map will be non-empty
data Map = Map {-# UNPACK #-}!Height Tree
  deriving stock (Eq,Ord,Show,Read,Generic)

instance NFData Map

-- | Get the underlying Tree
asBST           :: Map -> Tree
asBST (Map _ t) = t

-- | Height of the underlying BST
heightOf           :: Map -> Height
heightOf (Map h _) = h

--------------------------------------------------------------------------------

-- | construct the map
fromDescListPow2      :: Height -> NonEmpty (Key, Value) -> Map
fromDescListPow2 h xs = Map h $ Tree.fromDescListPow2 h xs
{-# INLINE fromDescListPow2 #-}


toAscList :: Map -> NonEmpty (Key,Value)
toAscList = Tree.toAscList . asBST

toDescList :: Map -> NonEmpty (Key,Value)
toDescList = Tree.toDescList . asBST


-- | Lookup a key in the map
lookup     :: Key -> Map -> Maybe Value
lookup q m = case lookupGE q m of
               Just (q',v) | q == q' -> Just v
               _                     -> Nothing
{-# INLINE lookup #-}

-- | Successor search
lookupGE   :: Key -> Map -> Maybe (Key, Value)
lookupGE q = Tree.lookupGE q . asBST
{-# INLINE lookupGE #-}

-- | Predecessor search
lookupLE          :: Key -> Map -> Maybe (Key, Value)
lookupLE q = Tree.lookupLE q . asBST
{-# INLINE lookupLE #-}

-- lookupGEDeleted           :: Key -> Map -> Maybe (Key, Value)
-- lookupGEDeleted q (Map t) = undefined leaf node t
--   where
--     leaf ek v   = case ek of
--                     StillHere k   | q <= k                  -> Just (k,v)
--                     DeletedAt t k | q <= k && t `after` now -> Just (k,v)
--                     _                                       -> Nothing
--     node l ek r = case ek of
--                     JustKey k | q <= k    -> l -- max in left subtree is k, so go left
--                               | otherwise -> r
--                     Infty -> r -- apparently the left tree is empty

--     after         :: Version -> Version -> Bool
--     t1 `after` t2 = t1 > t2

--     now = Version 0

-- lookupLEDeleted           :: Key -> Map -> Maybe (Key, Value)
-- lookupLEDeleted q (Map t) = undefined leaf node t
--   where
--     leaf ek v   = case ek of
--                     StillHere k   | k <= q                  -> Just (k,v)
--                     DeletedAt t k | k <= q && t `after` now -> Just (k,v)
--                     _                                       -> Nothing
--     node l ek r = case ek of
--                     JustKey k | k <= q    -> l
--                               | otherwise -> r <|> maxL -- TODO: this requires soem state.
--                     Infty -> r -- apparently the left tree is empty

--     after         :: Version -> Version -> Bool
--     t1 `after` t2 = t1 > t2

--     now = Version 0
--     maxL = undefined
