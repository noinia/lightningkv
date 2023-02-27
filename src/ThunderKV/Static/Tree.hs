module ThunderKV.Static.Tree
  ( Tree
  , FlatNode(..)
  , isLeaf
  , matchTree
  , asBinTree
  , toTree
  , fromNonEmpty
  , shiftRightBy
  ) where

import           Control.DeepSeq
import           Control.Monad.ST
import           Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.Array.ST as STArray
import qualified Data.Array.Unsafe as Array.Unsafe
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           GHC.Generics (Generic)
import           ThunderKV.BinTree
import           ThunderKV.Types

--------------------------------------------------------------------------------

-- | A binary tree embedded as a flat array.
type Tree = Array Index FlatNode

type STTree s = STArray.STArray s Index FlatNode



-- | The nodes of our embeded tree
data FlatNode = FlatLeaf {-# UNPACK #-} !Key {-# UNPACK #-} !Value
              | FlatNode {-# UNPACK #-} !Index
                         {-# UNPACK #-} !Key
                         {-# UNPACK #-} !Index
              deriving stock (Show,Read,Eq,Ord,Generic)
instance NFData FlatNode

-- | Test if this is a leaf or a node
isLeaf :: FlatNode -> Bool
isLeaf = \case
  FlatLeaf _ _ -> True
  _            -> False


-- | Construct a tree
toTree   :: Height -> NonEmpty (a,FlatNode) -> Tree
toTree h = fromNonEmpty h . fmap snd

-- | Given the height and the list of nodes, constructs the tree.
fromNonEmpty   :: Height -> NonEmpty FlatNode -> Tree
fromNonEmpty h = Array.listArray (0,(treeSize h) - 1) . NonEmpty.toList


-- | shifts the node to the right by some amount.
shiftRightBy       :: Index -> FlatNode -> FlatNode
shiftRightBy delta = \case
  FlatNode l k r -> FlatNode (l+delta) k (r+delta)
  n              -> n

--------------------------------------------------------------------------------

imapTreeWithAcc :: (Index -> Key -> Value -> (Key, Value,acc))
                -> (acc -> Key -> acc -> (Key, acc))
                -> Tree
                -> (Tree,acc)
imapTreeWithAcc = undefined

-- unsafeImapTreeWithAcc             :: (Index -> Key -> Value -> (Key, Value,acc))
--                                   -> (acc -> Key -> acc -> (Key, acc))
--                                   -> Tree
--                                   -> (Tree,acc)
-- unsafeImapTreeWithAcc leaf node t = runST ( Array.Unsafe.unsafeFreeze
--                                           . matchTreeM leaf node
--                                           . Array.Unsafe.unsafeThaw $ t
--                                           )

-- matchTreeM           :: (Index -> Key -> Value -> (Key, Value,acc))
--                      -> (acc -> Key -> acc -> (Key, acc))
--                      -> Array
--                      -> ST s (STTree s, acc)
-- matchTreeM leaf node = undefined

--------------------------------------------------------------------------------

-- | Pattern match on a Tree
matchTree             :: (Key -> Value -> r)
                      -> (r -> Key -> r -> r)
                      -> Tree
                      -> r
matchTree leaf node a = go 0
  where
    go i = case a Array.! i of
             FlatLeaf k v   -> leaf k v
             FlatNode l k r -> node (go l) k (go r)
{-# INLINE matchTree #-}

--------------------------------------------------------------------------------

-- | Convert the embedded tree into a normal Binary tree
asBinTree :: Tree -> BinTree Key Value
asBinTree = matchTree BinLeaf BinNode
{-# INLINE asBinTree #-}




-- matchTreeA             :: Applicative f
--                        => (Index -> Key -> Value -> f (Key,Value))
--                        -> (f () -> Key -> f () -> f Key)
--                        => Tree
--                        -> f Tree
-- matchTreeA leaf node a = go 0
--   where
--     go i = case a Array.! i of
--              FlatLeaf k v -> curry FlatLeaf <$> leaf i k v



-- -- | Overwrites the keys and values in the tree with the given ones.
-- -- assumes the keys are given in ascending order.
-- fillAsc      :: Tree' -> [(Key,Value)] -> Tree'
-- fillAsc t xs =
