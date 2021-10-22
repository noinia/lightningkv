{-# LANGUAGE ScopedTypeVariables #-}
module Thunder.Tree
  ( bimapTree, bimapNode

  , biTraverseTreeWithIndex
  , biTraverseTree
  , bifoldTree
  , biTraverseTreeM

  , fillWith'
  , TreeAB
  , TreeCD
  -- , A, B, C, D
  ) where

import qualified TreeAB
import qualified TreeCD
import Thunder.Tree.Types
import Thunder.Node

import qualified Data.Vector.Storable as SV
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.ST.Strict
import           Control.Monad.Trans.State.Strict ( StateT, evalStateT
                                                  , get, put
                                                  )
-- import           Data.Bifoldable
-- import           Data.Bifunctor
import           Data.Functor.Identity
import qualified Data.Vector.Storable.Mutable as SVM
import qualified A
import qualified B
import qualified C
import qualified D

--------------------------------------------------------------------------------

type TreeAB = TreeAB.Tree
type TreeCD = TreeCD.Tree

type A = A.NodeElem
type B = B.LeafElem
type C = C.NodeElem
type D = D.LeafElem

--------------------------------------------------------------------------------
-- * Mapping

-- | bimap implementation that uses vector's map rather than
-- fmap. This avoids a functor v constraint, which storable vectors do
-- not have.
--
-- O(scan(N))
bimapTree                     :: (A -> C) -> (B -> D) -> TreeAB l -> TreeCD l
bimapTree f g (TreeAB.Tree v) =
  TreeCD.Tree $ SV.generate (SV.length v) (\i -> bimapNode f g $ v SV.! i)

----------------------------------------
-- * Traversals

{-
 claim: biTraverseTree on VEB layout uses O(N/B) IOs.

 1) consider the two consecutive levels where subtrees have size >B
 and <B. (so a node fits in a "superblock" and in a "block" respectively)

 a block-leaf is a node whose children are not in the block.

 - for all non block-leaves in a subtree of size <B we don't need any
   IOs other than the at most 2 IOs just to load the block.

 - for all block leaves: (this means the current block is the
   root-block in one of the blocks one level above, and thus has
   actually Omega(B) children) consider the following
   block-replacement strategy:

   first process the left child-block of size B, then process the
   right, then process the current block: i.e. if we can maintain
   three blocks in memory this allows us to process the super block in
   O(B) IOs. (1/B IO per node of the tree)

   hence, we spend O(N/B^2 * B) = O(N/B) IOs at the superblock level.

   an optimal cache replacement algo is at leat as good as the one sketched here.
*
  2) for any higher levels there are only few subtrees anyway; just
  charge one to every subtree.

  TODO: detals


  FIXME: So far this reasons only about the "going up" part. Not about the "going down" part.

-}
biTraverseTreeWithIndex                           :: Monad f
                                                  => (Index -> Index -> x -> A -> Index -> x -> f x)
                                                  -> (Index -> B -> f x)
                                                  -> TreeAB l -> f x
biTraverseTreeWithIndex node leaf (TreeAB.Tree v) = go 0
  where
    go i = case v SV.! fromIntegral i of
             TreeAB.Leaf x     -> leaf i x
             TreeAB.Node l k r -> do l' <- go l
                                     r' <- go r
                                     node i l l' k r r'


biTraverseTree                :: Monad f
                              => (x -> A -> x -> f x) -> (B -> f x) -> TreeAB l -> f x
biTraverseTree node leaf =
  biTraverseTreeWithIndex (\_ _ l k _ r -> node l k r) (const leaf)


bifoldTree                    :: (x -> A -> x -> x) -> (B -> x) -> TreeAB l -> x
bifoldTree node leaf = runIdentity
                     . biTraverseTree (\l k r -> Identity $ node l k r) (Identity . leaf)


-- | traverse the tree bottom up constructing a new tree
biTraverseTreeM                     :: forall m l.
                                       PrimMonad m
                                    => (Index -> C -> A -> C -> m C)
                                    -- ^ how to create the c in an internal node given the
                                    --   index of the node in the Vector, the c from its left child
                                    --   the current a, and the c from its right child.
                                   -> (Index -> B -> m D)
                                   -- ^ how to create a d in a leaf (given the index of the leaf and
                                   -- the current b)
                                   -> (D -> m C) -- ^ function to lift d's into cs
                                   -> TreeAB l
                                   -> m (TreeCD l)
biTraverseTreeM node leaf liftCD t = do w <- SVM.new (fromIntegral n)
                                        _ <- biTraverseTreeWithIndex (node' w) (leaf' w) t
                                        -- discarded result is the c at the root.
                                        TreeCD.Tree <$> SV.unsafeFreeze w
  where
    -- leaf' :: Index -> b -> m (Node c d)
    -- leaf' i b = Leaf <$> leaf i b

    -- node' :: Index -> Index -> c -> a -> Index -> c -> m (Node c d)
    -- node' i li l k ri r = (\c' -> Node li c' ri) <$> node i l k r

    leaf' w i b = do d <- leaf i b
                     SVM.write w (fromIntegral i) $ TreeCD.Leaf d
                     liftCD d

    node' w i li l k ri r = do c <- node i l k r
                               SVM.write w (fromIntegral i) $ TreeCD.Node li c ri
                               pure c

    n = TreeAB.numNodes t


--------------------------------------------------------------------------------
-- * Filling Binary Search Trees


type SST s cs = StateT cs (ST s)

-- | More general version of fillWith that allows us to specify how to
-- construct a node, how to create a leaf, and how to lift a leaf into
-- a c.
fillWith'                  :: forall x l. (C -> A -> C -> C) -- ^ node combinator
                           -> (x -> B -> D)      -- ^ leaf builder
                           -> (D -> C)           -- ^ lift a leaf into c
                           -> [x]
                           -> TreeAB l -> TreeCD l
fillWith' node leaf f xs t = runST (flip evalStateT xs $ biTraverseTreeM node' leaf' lift' t)
  where
    node'         :: Index -> C -> A -> C -> SST s [x] C
    node' _ l a r = pure $ node l a r

    leaf'     :: Index -> B -> SST s [x] D
    leaf' _ b = get >>= \case
                  []      -> error "fillWith: too few elements"
                  (x:xs') -> do put xs'
                                pure $ leaf x b

    lift' :: D -> SST s [x] C
    lift' = pure . f
