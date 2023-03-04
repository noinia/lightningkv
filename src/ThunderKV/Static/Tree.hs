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
  , fillDesc

  , minimum
  , maximum
  ) where

import           Control.DeepSeq
import           Control.Monad.ST
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
-- import           Foreign.Storable
import           Foreign.Storable.Generic
import           Foreign.Storable
import           GHC.Generics (Generic)
import           ThunderKV.Static.BinTree
import           ThunderKV.Static.Types
import           ThunderKV.LargeArray (LargeArray)
import qualified ThunderKV.LargeArray as LargeArray
import           Prelude hiding (minimum,maximum)
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
-- assumes the keys are given in descending order.
fillDesc               :: [(Key,Value)] -> Tree -> Tree
fillDesc xs (Tree arr) = Tree
                       . LargeArray.mapR (\(NodeWithMax n _) -> n)
                       $ runST $ do mArr <- LargeArray.unsafeNew n
                                    go mArr xs (LargeArray.indicesRL arr)
                                    LargeArray.unsafeFreeze mArr
  where
    n = LargeArray.length arr

    go          :: forall s. LargeArray.MLargeArray s NodeWithMax
                -> [(Key,Value)]
                -> [Index] -> ST s ()
    go mArr kvs = \case
        []     -> pure ()
        (i:is) -> do (kvs',x) <- f i kvs (arr LargeArray.! i)
                     LargeArray.write mArr i x
                     go mArr kvs' is
      where
        f       :: Index -> [(Key,Value)] -> FlatNode -> ST s ([(Key,Value)], NodeWithMax)
        f i kvs = \case
          FlatLeaf _ _   -> pure $ case kvs of
                              []         -> error "fillDesc: too few elements"
                              (k,v):kvs' -> (kvs', NodeWithMax (FlatLeaf k v) k)
          FlatNode l _ r -> do NodeWithMax _ m <- LargeArray.read mArr r
                               NodeWithMax _ k <- LargeArray.read mArr l
                               pure $ (kvs, NodeWithMax (FlatNode l k r) m)

    -- The main idea is to traverse the old array right to left, while building a new array
    -- that stores 'NodeWithMax'es, i.e. the new nodes as well as their subtree maxes.
    -- since every node occurs after (i.e. with an smaller index) than its two children
    -- we can thus compute the subtree maxes in the same scan as replacing the leaves.
    --
    -- moreover, the leaves appear in sorted order in the array. So we
    -- can just replace them from right to left by elements in decreasing order.





        --

  -- . snd $ LargeArray.imapAccumL f xs arr
  -- where
  --   f _ kvs = \case
  --     FlatLeaf _ _   -> case kvs of
  --                         []         -> error "fillAsc: Too few elements!!"
  --                         (k,v):kvs' -> (kvs',FlatLeaf k v)
  --     node           -> (kvs, node)
  -- todo; fold te keys
-- FIXME: if you get the keys in decreasin order, we can fuse the scan; i.e. so that we need only 2 passes rather than 3

data NodeWithMax = NodeWithMax {-# UNPACK #-}!FlatNode {-# UNPACK #-}!Key
  deriving (Show,Generic,GStorable)

-- unsafeRecomputeKeys            :: Tree -> Tree
-- unsafeRecomputeKeys (Tree arr) = Tree . snd $

--   LargeArray.mapR f arr
--   where
--     f = \case
--       leaf@(FlatLeaf k _) -> NodeWithMax leaf k
--       FlatNode l _ r      -> let k =
--                                  m = arr ! r
--                              in NodeWithMax (FlatNode l k r)




-- fillAsc xs t = imapTreeWithAcc t node leaf
--   where






-- imapTreeWithAcc :: (Index -> Key -> Value -> (Key, Value,acc))
--                 -> (acc -> Key -> acc -> (Key, acc))
--                 -> Tree
--                 -> (Tree,acc)
-- imapTreeWithAcc = undefined

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

-- | gets the largest key,value pair in the tree
maximum :: Tree -> (Key,Value)
maximum = matchTree (\k v -> (k,v)) (\_ _ r -> r)

-- | gets the smallest key,value pair in the tree.
minimum :: Tree -> (Key,Value)
minimum = matchTree (\k v -> (k,v)) (\l _ _ -> l)
