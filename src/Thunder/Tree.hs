{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.Tree
  ( GTree(..) -- TODO: don't export constructor
  , Tree
  , Layout(..)

  , toBinTree
  , numLeaves, numNodes

  , biTraverseTree
  , bifoldTree
  , bimapTree

  , biTraverseTreeM

  , leaves
  , searchLeafR


  , Index
  , Node
  , WithMax

  , Height, Size


  , reFillWith
  , fillWith, fillWith'


  , printTree
  ) where

import           Control.DeepSeq
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.ST.Strict
import           Control.Monad.Trans.State.Strict ( StateT, evalStateT
                                                  , get, put
                                                  )
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Functor.Identity
import qualified Data.Tree as DataTree
import qualified Data.Tree.View as TreeView
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GVM
import           Foreign.Storable.Generic
import           GHC.Generics
-- -- import           Data.Word (Word8)

-- import           Data.Store
-- import           Data.Store.TH
import           Thunder.BinTree (BinTree(..))
import           Thunder.Node
-- -- import           Foreign.Storable
-- -- import           Foreign.Ptr


--------------------------------------------------------------------------------

data Layout = VEB
            | InOrder

type Height = Int

type Size = Int

--------------------------------------------------------------------------------

-- | A Tree whose leaves store values of type b, whose internal nodes
-- store values of type a. The tree is stored in an Vector v using
-- layout l.
newtype GTree (l :: Layout) v a b = Tree (v (Node a b))
  deriving (Generic)

type Tree l = GTree l V.Vector

deriving instance (Show a, Show b, Show (v (Node a b))) => Show (GTree l v a b)
deriving instance (Eq a, Eq b, Eq (v (Node a b)))       => Eq (GTree l v a b)

instance NFData (v (Node a b)) => NFData (GTree l v a b)

instance Functor v => Bifunctor (GTree l v) where
  bimap f g (Tree v) = Tree $ fmap (bimap f g) v





-- | Convert into a BinTree
toBinTree :: GV.Vector v (Node a b) => GTree l v a b -> BinTree a b
toBinTree (Tree t) = go 0
  where
    go i = case t GV.! fromIntegral i of
             Leaf x     -> BinLeaf x
             Node l k r -> BinNode (go l) k (go r)

-- | Get the number of nodes in the tree
numNodes          :: GV.Vector v (Node a b) => GTree l v a b -> Int
numNodes (Tree v) = GV.length v

-- | get the capacity, i.e. the number of leaves of the tree
numLeaves   :: GV.Vector v (Node a b) => GTree l v a b -> Int
numLeaves t = (numNodes t + 1) `div` 2


-- | bimap implementation that uses vector's map rather than
-- fmap. This avoids a functor v constraint, which storable vectors do
-- not have.
bimapTree              :: ( GV.Vector v (Node a b)
                          , GV.Vector v (Node c d)
                          )
                       => (a -> c) -> (b -> d) -> GTree l v a b -> GTree l v c d
bimapTree f g (Tree v) = Tree $ GV.map (bimap f g) v


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
biTraverseTreeWithIndex                    :: (GV.Vector v (Node a b), Monad f)
                                           => (Index -> Index -> c -> a -> Index -> c -> f c)
                                           -> (Index -> b -> f c)
                                           -> GTree l v a b -> f c
biTraverseTreeWithIndex node leaf (Tree v) = go 0
  where
    go i = case v GV.! fromIntegral i of
             Leaf x     -> leaf i x
             Node l k r -> do l' <- go l
                              r' <- go r
                              node i l l' k r r'

biTraverseTree                :: (GV.Vector v (Node a b), Monad f)
                              => (c -> a -> c -> f c) -> (b -> f c) -> GTree l v a b -> f c
biTraverseTree node leaf =
  biTraverseTreeWithIndex (\_ _ l k _ r -> node l k r) (const leaf)


bifoldTree                    :: GV.Vector v (Node a b)
                              => (c -> a -> c -> c) -> (b -> c) -> GTree l v a b -> c
bifoldTree node leaf = runIdentity
                     . biTraverseTree (\l k r -> Identity $ node l k r) (Identity . leaf)

instance Bifoldable (Tree l) where
  bifoldMap f = bifoldTree (\l k r -> l <> f k <> r)



biTraverseTreeM                     :: forall v w a b c d m l.
                                       ( GV.Vector v (Node a b)
                                       , GV.Vector w (Node c d)
                                       , PrimMonad m
                                       )
                                    => (Index -> c -> a -> c -> m c)
                                    -- ^ how to create the c in an internal node given the
                                    --   index of the node in the Vector, the c from its left child
                                    --   the current a, and the c from its right child.
                                   -> (Index -> b -> m d)
                                   -- ^ how to create a d in a leaf (given the index of the leaf and
                                   -- the current b)
                                   -> (d -> m c) -- ^ function to lift d's into cs
                                   -> GTree l v a b
                                   -> m (GTree l w c d)
biTraverseTreeM node leaf liftCD t = do w <- GVM.new (fromIntegral n)
                                        _ <- biTraverseTreeWithIndex (node' w) (leaf' w) t
                                        -- discarded result is the c at the root.
                                        Tree <$> GV.unsafeFreeze w
  where
    -- leaf' :: Index -> b -> m (Node c d)
    -- leaf' i b = Leaf <$> leaf i b

    -- node' :: Index -> Index -> c -> a -> Index -> c -> m (Node c d)
    -- node' i li l k ri r = (\c' -> Node li c' ri) <$> node i l k r

    leaf' w i b = do d <- leaf i b
                     GVM.write w (fromIntegral i) $ Leaf d
                     liftCD d

    node' w i li l k ri r = do c <- node i l k r
                               GVM.write w (fromIntegral i) $ Node li c ri
                               pure c

    n = numNodes t




  -- runPrimMonad foo

-- foo   :: ( PrimMonad m
--          , GV.Vector v (Node a b)
--          , GVM.MVector w c
--          )
--       => GTree l v a b
--       -> (Index -> c -> a -> c -> c)
--       -> (Index -> b -> c)
--       -> m (w (PrimState m) c)
-- foo t@(Tree v) node leaf = do w <- GVM.new (fromIntegral n)
--                               biTraverseTreeWithIndex (node' w) (leaf' w) t
--                               pure w
--   where
--     n = GV.length v

--     leaf' w i b     = GVM.write w (fromIntegral i) $ leaf i b
--     node' w i l k r = GVM.write w (fromIntegral i) $ node i l k r

--------------------------------------------------------------------------------
-- * Traversals

-- | Get the values in the leaves in ascending order.
leaves          :: (GV.Vector v (Node a b), GV.Vector v b) => GTree l v a b -> v b
leaves (Tree t) = GV.mapMaybe (\case
                                  Leaf x -> Just x
                                  _      -> Nothing) t

--------------------------------------------------------------------------------
-- * Querying

-- | Binary-search on a tree. the predicate indicates if we should go right
searchLeafR                  :: GV.Vector v (Node a b) => (a -> Bool) -> GTree l v a b -> b
searchLeafR goRight (Tree t) = go 0
  where
    go i = case t GV.! fromIntegral i of
             Leaf v       -> v
             Node li k ri -> if goRight k then go ri else go li



-- -- | Find the strict successor of k, if it exists
-- lookupGT   :: Ord k => k -> Tree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
-- lookupGT q = lookup' (> Just q)

-- -- | Find k or the the successor of k, if it exists
-- lookupGE   :: Ord k => k -> Tree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
-- lookupGE q = lookup' (>= Just q)



-- -- lookupLT   :: Ord k => k -> Tree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
-- -- lookupLT q = lookup' (< Just q)

-- -- lookupLE   :: Ord k => k -> Tree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
-- -- lookupLE q = lookup' (<= Just q)




-- -- | Helper function to implement the lookups
-- lookup'     :: (Maybe k -> Bool) -> Tree (Maybe k) (Maybe (k,v)) -> Maybe (k,v)
-- lookup' p t = let lf = searchLeafR p t
--               in if p (fst <$> lf) then lf else Nothing


--------------------------------------------------------------------------------
-- * Filling Binary Search Trees

-- | Given a leaf to value function, an ascending list of elements xs,
-- and a tree t, create a BST by overwriting all values in the BST.
--
-- pre: length xs >= capacity t
reFillWith      :: ( GV.Vector v (Node a b)
                   , GV.Vector w (Node (WithMax c) d)
                   , GV.Vector w (Node c d)
                   )
                => (d -> c) -> [d] -> GTree l v a b -> GTree l w c d
reFillWith f xs = bimapTree (\(WithMax x _) -> x) id . fillWith f xs

-- | Given a leaf to key function and a list of leaf values,
-- "overwrite" the values in the input tree by those in the input list
-- (in increasing order). Note that this actually constructs a new
-- array in memory sotring this tree.
--
-- This function is useful when the input tree has the right
-- structure/memory layout.
fillWith   :: ( GV.Vector v (Node a b)
              , GV.Vector w (Node (WithMax c) d)
              )
           => (d -> c) -> [d] -> GTree l v a b -> GTree l w (WithMax c) d
fillWith f = fillWith' (\(WithMax _ lM) _ (WithMax _ rM) -> WithMax lM rM)
                       (\d _ -> d)
                       (\d -> let c = f d in WithMax c c)

data WithMax c = WithMax {-# UNBOX #-} !c
                         {-# UNBOX #-} !c -- the maximum
               deriving stock (Show,Eq,Ord,Generic)
               deriving anyclass (GStorable)

type SST s cs = StateT cs (ST s)

-- | More general version of fillWith that allows us to specify how to
-- construct a node, how to create a leaf, and how to lift a leaf into
-- a c.
fillWith'                  :: forall v w a b c d x l.
                           ( GV.Vector v (Node a b)
                           , GV.Vector w (Node c d)
                           )
                          => (c -> a -> c -> c) -- ^ node combinator
                          -> (x -> b -> d)      -- ^ leaf builder
                          -> (d -> c)           -- ^ lift a leaf into c
                          -> [x]
                          -> GTree l v a b -> GTree l w c d
fillWith' node leaf f xs t = runST (flip evalStateT xs $ biTraverseTreeM node' leaf' lift' t)
  where
    node'         :: Index -> c -> a -> c -> SST s [x] c
    node' _ l a r = pure $ node l a r

    leaf'     :: Index -> b -> SST s [x] d
    leaf' _ b = get >>= \case
                  []      -> error "fillWith: too few elements"
                  (x:xs') -> do put xs'
                                pure $ leaf x b

    lift' :: d -> SST s [x] c
    lift' = pure . f

--------------------------------------------------------------------------------
-- * Debugging

printTree :: (Show a, Show b) => Tree l a b -> IO ()
printTree = TreeView.drawTree . convert . toBinTree
  where
    convert (BinLeaf x) = DataTree.Node (show x) []
    convert (BinNode l k r) = DataTree.Node (show k) [convert l, convert r]

--------------------------------------------------------------------------------
