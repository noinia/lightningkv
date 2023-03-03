{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}
{-# LANGUAGE DeriveAnyClass #-}
module ThunderKV.Static.Tree
  ( Tree
  , asArray
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
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
-- import           Foreign.Storable
import           Foreign.Storable.Generic
import           GHC.Generics (Generic)
import           ThunderKV.Static.BinTree
import           ThunderKV.Static.Types
import           ThunderKV.LargeArray (LargeArray)
import qualified ThunderKV.LargeArray as LargeArray

--------------------------------------------------------------------------------


-- | A binary tree embedded as a flat array.
newtype Tree = Tree (LargeArray FlatNode)
  deriving stock (Show,Read,Eq,Ord,Generic)
  -- deriving newtype (Storable)
  -- deriving anyclass (GStorable)

            -- Array Index FlatNode

instance NFData Tree

-- | Get the flat array storing the nodes
asArray          :: Tree -> LargeArray FlatNode
asArray (Tree a) = a


-- type STTree s = STArray.STArray s Index FlatNode


-- | The nodes of our embeded tree
data FlatNode = FlatLeaf {-# UNPACK #-} !Key {-# UNPACK #-} !Value
              | FlatNode {-# UNPACK #-} !Index
                         {-# UNPACK #-} !Key
                         {-# UNPACK #-} !Index
              deriving stock (Show,Read,Eq,Ord,Generic)
              deriving anyclass (GStorable)
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
fromNonEmpty h = Tree . LargeArray.fromListN (size h) . NonEmpty.toList
  -- indices in the range [0,size h - 1]

-- | shifts the node to the right by some amount.
shiftRightBy       :: Index -> FlatNode -> FlatNode
shiftRightBy delta = \case
  FlatNode l k r -> FlatNode (l+delta) k (r+delta)
  n              -> n

--------------------------------------------------------------------------------

-- | Overwrites the keys and values in the tree with the given ones.
-- assumes the keys are given in ascending order.
fillAsc               :: [(Key,Value)] -> Tree -> Tree
fillAsc xs (Tree arr) = Tree . snd $ LargeArray.imapAccumL f xs arr
  where
    f i kvs = \case
      FlatLeaf _ _   -> case kvs of
                          []         -> error "fillAsc: Too few elements!!"
                          (k,v):kvs' -> (kvs',FlatLeaf k v)
      node           -> (kvs, node)

-- fillAsc xs t = imapTreeWithAcc t node leaf
--   where






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
matchTree                    :: (Key -> Value -> r)
                             -> (r -> Key -> r -> r)
                             -> Tree
                             -> r
matchTree leaf node (Tree a) = go 0
  where
    go i = case a LargeArray.! i of
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
--     go i = case a Vector.! i of
--              FlatLeaf k v -> curry FlatLeaf <$> leaf i k v



-- -- | Overwrites the keys and values in the tree with the given ones.
-- -- assumes the keys are given in ascending order.
-- fillAsc      :: Tree' -> [(Key,Value)] -> Tree'
-- fillAsc t xs =


--------------------------------------------------------------------------------
