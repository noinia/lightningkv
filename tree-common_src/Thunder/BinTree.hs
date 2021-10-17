module Thunder.BinTree
  ( BinTree(..)
  , foldBinTree
  , leaves, height
  , levels
  , binarySearch, binarySearchLeaf
  ) where

import           Data.Bifunctor


--------------------------------------------------------------------------------

type Height = Int

--------------------------------------------------------------------------------

data BinTree a b = BinLeaf b | BinNode (BinTree a b) a (BinTree a b)
                 deriving (Show,Eq)

instance Bifunctor BinTree where
  bimap f g = \case
    BinLeaf x     -> BinLeaf $ g x
    BinNode l k r -> BinNode (bimap f g l) (f k) (bimap f g r)


foldBinTree           :: (b -> c) -> (c -> a -> c -> c) -> BinTree a b -> c
foldBinTree leaf node = go
  where
    go = \case
      BinLeaf v     -> leaf v
      BinNode l k r -> node (go l) k (go r)

leaves :: BinTree a b -> [b]
leaves = foldBinTree (:[]) (\l _ r -> l <> r)

height :: BinTree a b -> Height
height = foldBinTree (const 0) (\l _ r -> 1 + max l r)

-- | Essentially a BFS traversal of the tree
levels   :: BinTree a b -> [[BinTree a b]]
levels = init . levels'
  where
    levels' t = [t] : case t of
      BinLeaf _     -> [[]]
      BinNode l _ r -> zipWith (<>) (levels' l) (levels' r)


--------------------------------------------------------------------------------

-- | Version of binary search that also actually checks the leaf that
-- we obtain
binarySearch     :: (a -> Bool) -> BinTree a a -> Maybe a
binarySearch p t = let v = binarySearchLeaf p t
                   in if p v then Just v else Nothing

-- | If we satisfy the predicate, go left, otherwise go right.
binarySearchLeaf   :: (a -> Bool) -> BinTree a b -> b
binarySearchLeaf p = go
  where
    go = \case
      BinLeaf v                 -> v
      BinNode l k r | p k       -> go l
                    | otherwise -> go r
