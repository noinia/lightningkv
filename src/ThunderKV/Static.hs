module ThunderKV.Static
  ( Map
  , fromAscListPow2
  , lookup
  , lookupGE
  ) where

import           Control.DeepSeq
-- import           Control.Monad.Writer
import           Data.Array (Array)
import qualified Data.Array as Array
import           ThunderKV.Types
import           ThunderKV.Prokob
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Word
import           GHC.Generics (Generic)
import           Prelude hiding (lookup)

--------------------------------------------------------------------------------

newtype Map = Map Tree
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,NFData)

--------------------------------------------------------------------------------

fromAscListPow2 :: [(Key, Value)] -> Map
fromAscListPow2 = undefined


lookup     :: Key -> Map -> Maybe Value
lookup q m = case lookupGE q m of
               Just (q',v) | q == q' -> Just v
               _                     -> Nothing
{-# INLINE lookup #-}


lookupGE          :: Key -> Map -> Maybe (Key, Value)
lookupGE q (Map t) = matchTree leaf node t
  where
    leaf k v | q <= k    = Just (k,v)
             | otherwise = Nothing
    node l k r | q <= k    = l
               | otherwise = r
{-# INLINE lookupGE #-}

lookupLE          :: Key -> Map -> Maybe (Key, Value)
lookupLE q (Map t) = matchTree leaf node t LEMode
  where
    leaf                 :: Key -> Value -> LEMode -> Maybe (Key,Value)
    leaf k v = \case
      MaxMode            -> Just (k,v)
      LEMode | k <= q    -> Just (k,v)
             | otherwise -> Nothing

    node                  :: (LEMode -> Maybe (Key,Value))
                          -> Key
                          -> (LEMode -> Maybe (Key,Value))
                          -> LEMode -> Maybe (Key,Value)
    node l k r = \case
      MaxMode            -> r MaxMode
      LEMode | q <= k    -> l LEMode
             | otherwise -> r LEMode <|> l MaxMode
{-# INLINE lookupGE #-}

data LEMode = LEMode | MaxMode

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


--------------------------------------------------------------------------------

-- arrayFromList :: [(Index,a)] -> []



-- flatTest :: Tree
flatTest = layoutWith Key Value test

flatSimplest = layoutWith Key Value simplest


simplest :: BinTree Word64 Word64
simplest = BinNode (BinLeaf 0 1) 0 (BinLeaf 5 6)
