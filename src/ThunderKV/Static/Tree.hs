{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}
{-# LANGUAGE DeriveAnyClass #-}
module ThunderKV.Static.Tree
  ( Tree
  , SubTree
  , asArray
  , asSubTree
  , toAscList, toDescList
  , FlatNode(..)
  , isLeaf
  , matchTree
  , asBinTree
  , toTree
  , fromNonEmpty
  , shiftRightBy
  , fillDesc
  , lookupGE
  , lookupLE

  , minimum
  , maximum
  ) where

import           Control.Applicative ((<|>))
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
import           Prelude hiding (minimum,maximum)
--------------------------------------------------------------------------------


-- | A binary tree embedded as a flat array.
newtype Tree = Tree (LargeArray FlatNode)
  deriving stock (Show,Read,Eq,Ord,Generic)
  -- deriving newtype (Storable)
  -- deriving anyclass (GStorable)

-- | Subtree of a binary tree.
data SubTree = SubTree {-# UNPACK #-}!Index -- index of the root of the subtree
                       {-#UNPACK #-}!Tree
  deriving stock (Show,Read,Eq,Ord,Generic)

instance NFData Tree
instance NFData SubTree


-- | Get the flat array storing the nodes
asArray          :: Tree -> LargeArray FlatNode
asArray (Tree a) = a

-- | convert into a subtree (that just represents the entire tree)
asSubTree :: Tree -> SubTree
asSubTree = SubTree 0


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
                       . LargeArray.mapr (\(NodeWithMax n' _) -> n')
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
        f _ kvs' = \case
          FlatLeaf _ _   -> pure $ case kvs' of
                              []          -> error "fillDesc: too few elements"
                              (k,v):kvs'' -> (kvs'', NodeWithMax (FlatLeaf k v) k)
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

-- | Node that also stores a subtree maximum.
data NodeWithMax = NodeWithMax !FlatNode {-# UNPACK #-}!Key
  deriving (Show,Generic,GStorable)

-- FIXME: we should make sure to unpack the flatnode!!!


--------------------------------------------------------------------------------

-- | Pattern match on a Tree
matchTree           :: (Key -> Value -> r)
                    -> (r -> Key -> r -> r)
                    -> Tree
                    -> r
matchTree leaf node = matchSubTree leaf node . SubTree 0
{-# INLINE matchTree #-}

-- | on subtree
matchSubTree                                 :: (Key -> Value -> r)
                                             -> (r -> Key -> r -> r)
                                             -> SubTree
                                             -> r
matchSubTree leaf node (SubTree r0  (Tree a)) = go r0
  where
    go i = case a LargeArray.! i of
             FlatLeaf k v   -> leaf k v
             FlatNode l k r -> node (go l) k (go r)
{-# INLINE matchSubTree #-}


--------------------------------------------------------------------------------

-- | Convert the embedded tree into a normal Binary tree
asBinTree :: Tree -> BinTree Key Value
asBinTree = matchTree BinLeaf BinNode
{-# INLINE asBinTree #-}

--------------------------------------------------------------------------------

-- | gets the largest key,value pair in the tree
maximum :: SubTree -> (Key,Value)
maximum = matchSubTree (\k v -> (k,v)) (\_ _ r -> r)

-- | gets the smallest key,value pair in the tree.
minimum :: SubTree -> (Key,Value)
minimum = matchSubTree (\k v -> (k,v)) (\l _ _ -> l)

-- | Successor search
lookupGE   :: Key -> Tree -> Maybe (Key, Value)
lookupGE q = matchTree leaf node
  where
    leaf k v | q <= k    = Just (k,v)
             | otherwise = Nothing
    node l k r | q <= k    = l
               | otherwise = r
{-# INLINE lookupGE #-}

-- | Predecessor search
lookupLE                :: Key -> Tree -> Maybe (Key, Value)
lookupLE q t@(Tree arr) = go 0
  where
    go i = case arr LargeArray.! i of
      FlatLeaf k v | k <= q    -> Just (k,v)
                   | otherwise -> Nothing
      FlatNode l k r | q <= k  -> go l
                 | otherwise   -> go r <|> Just (maximum $ SubTree l t)
{-# INLINE lookupLE #-}

--------------------------------------------------------------------------------


toAscList :: Tree -> NonEmpty (Key,Value)
toAscList = NonEmpty.fromList . LargeArray.foldr (\n acc -> extractLeaf n `consM` acc) [] . asArray


toDescList :: Tree -> NonEmpty (Key,Value)
toDescList = NonEmpty.fromList . LargeArray.foldl (\acc n -> extractLeaf n `consM` acc) [] . asArray

consM       :: Maybe a -> [a] -> [a]
consM mh tl = maybe tl (:tl) mh

extractLeaf :: FlatNode -> Maybe (Key,Value)
extractLeaf = \case
  FlatLeaf k v -> Just (k,v)
  _            -> Nothing
