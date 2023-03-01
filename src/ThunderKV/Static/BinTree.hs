module ThunderKV.Static.BinTree
  ( BinTree(..)
  , heightL
  , leavesWithIndex
  , itraverseWithKey
  , imapLeavesWithKey
  , fromAscListPow2
  ) where

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Foldable.WithIndex
import           Data.Functor.Identity
import           Data.Functor.WithIndex
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Traversable.WithIndex
import           ThunderKV.Static.Types

--------------------------------------------------------------------------------

-- | Normal binary leaf trees
data BinTree k v = BinLeaf !k !v
                 | BinNode (BinTree k v) !k (BinTree k v)
                 deriving (Show,Eq,Functor,Foldable,Traversable)

instance Bifunctor BinTree where
  bimap f g = go
    where
      go = \case
        BinLeaf k v   -> BinLeaf (f k) (g v)
        BinNode l k r -> BinNode (go l) (f k) (go r)

instance Bifoldable BinTree where
  bifoldMap f g = go
    where
      go = \case
        BinLeaf k v   -> f k <> g v
        BinNode l k r -> go l <> f k <> go r

instance Bitraversable BinTree where
  bitraverse f g = go
    where
      go = \case
        BinLeaf k v   -> BinLeaf <$> f k <*> g v
        BinNode l k r -> BinNode <$> go l <*> f k <*> go r

instance FunctorWithIndex Index (BinTree k)
instance FoldableWithIndex Index (BinTree k)
instance TraversableWithIndex Index (BinTree k) where
  itraverse f = itraverseWithKey (\i _ v -> f i v)
  {-# INLINE itraverse #-}

-- | Traverse the leaves of the tree, in-order, applying a function on
-- both the index (i.e. the how manyth) leaf this is as well as the
-- key.
itraverseWithKey     :: forall f k v v'. Applicative f
                     => (Index -> k -> v -> f v') -> BinTree k v -> f (BinTree k v')
itraverseWithKey f t = t'
    where
      WithCount _ t' = go 0 t
      go   :: Index -> BinTree k v -> WithCount (f (BinTree k v'))
      go s = \case
        BinLeaf k v   -> WithCount (s+1) (BinLeaf k <$> f s k v)
        BinNode l k r -> let WithCount m tl = go s l
                             WithCount n tr = go m r
                         in WithCount n ((\l' r' -> BinNode l' k r') <$> tl <*> tr)
{-# INLINE itraverseWithKey #-}

data WithCount a = WithCount {-# UNPACK #-} !Index a


-- | Constructs a complete bin-tree
--
-- pre: input has size 2^h, for some h >= 0
fromAscListPow2 :: Ord k => NonEmpty (k,v) -> BinTree k v
fromAscListPow2 = fst . repeatedly merge . fmap (\(k,v) -> (BinLeaf k v, k))
  where
    repeatedly _ (t :| []) = t
    repeatedly f ts        = repeatedly f $ f ts

    merge ts@(_ :| [])  = ts
    merge (l :| r : []) = node l r :| []
    merge (l :| r : ts) = node l r NonEmpty.<| merge (NonEmpty.fromList ts)

    node (l,k) (r,m) = (BinNode l k r, m)
{-# INLINE fromAscListPow2 #-}

-- | Report all leaves with their index.
leavesWithIndex :: BinTree k v -> [(Index, v)]
leavesWithIndex = ifoldMap (\i v -> [(i,v)])
{-# INLINE leavesWithIndex #-}

-- | Maps a function over all leaves with both their index and their key.
imapLeavesWithKey   :: (Index -> k -> v -> v')
                    -> BinTree k v
                    -> BinTree k v'
imapLeavesWithKey f = runIdentity . itraverseWithKey (\i k v -> Identity $ f i k v)


-- | Computes the number of nodes h on the left-spine.
--
-- Assuming the tree is a complete binary tree, this then means the
-- tree has 2^h nodes.
heightL :: BinTree k v -> Height
heightL = \case
  BinLeaf   _ _ -> 0
  BinNode l _ _ -> 1 + heightL l
