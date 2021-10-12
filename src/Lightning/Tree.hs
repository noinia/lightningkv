{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lightning.Tree
  ( GTree(..) -- TODO: don't export constructor
  , Tree
  , Layout(..)

  , toBinTree

  , biTraverseTree
  , bifoldTree
  , bimapTree

  , biTraverseTreeM

  , searchLeafR

  , Index

  , Node(..)


  ) where

import           Control.DeepSeq
import           Control.Monad.Primitive (PrimMonad)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Functor.Identity
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GVM
import           GHC.Generics
-- import           Data.Store
-- import           Data.Store.TH
import           Lightning.BinTree

--------------------------------------------------------------------------------

type Index  = Word

data Node a b = Leaf {-# UNPACK #-} !b
              | Node {-# UNPACK #-} !Index
                     {-# UNPACK #-} !a
                     {-# UNPACK #-} !Index
              deriving (Show,Eq,Ord,Generic,Generic1)

-- $($(derive [d|
--     instance (Store a, Store b) => Deriving (Store (Node a b))
--     |]))

instance (NFData a, NFData b) => NFData (Node a b) where rnf = rnf1
instance NFData a => NFData1 (Node a)

instance Bifunctor Node where
  bimap f g = \case
    Leaf x     -> Leaf (g x)
    Node l k r -> Node l (f k) r


--------------------------------------------------------------------------------

data Layout = VEB
            | InOrder


--------------------------------------------------------------------------------


newtype GTree (l :: Layout) v a b = Tree (v (Node a b))

deriving instance (Show a, Show b, Show (v (Node a b))) => Show (GTree l v a b)
deriving instance (Eq a, Eq b, Eq (v (Node a b)))       => Eq (GTree l v a b)


toBinTree :: GV.Vector v (Node a b) => GTree l v a b -> BinTree a b
toBinTree (Tree t) = go 0
  where
    go i = case t GV.! fromIntegral i of
             Leaf x     -> BinLeaf x
             Node l k r -> BinNode (go l) k (go r)


type Tree l = GTree l V.Vector


numNodes          :: GV.Vector v (Node a b) => GTree l v a b -> Int
numNodes (Tree v) = GV.length v



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


bimapTree              :: ( GV.Vector v (Node a b)
                          , GV.Vector v (Node c d)
                          )
                       => (a -> c) -> (b -> d) -> GTree l v a b -> GTree l v c d
bimapTree f g (Tree v) = Tree $ GV.map (bimap f g) v


instance Bifoldable (Tree l) where
  bifoldMap f = bifoldTree (\l k r -> l <> f k <> r)
instance Bifunctor (Tree l) where
  bimap = bimapTree



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

--------------------------------------------------------------------------------
-- * Querying

-- | Binary-search on a tree. the predicate indicates if we should go right
searchLeafR                  :: GV.Vector v (Node a b) => (a -> Bool) -> GTree l v a b -> b
searchLeafR goRight (Tree t) = go 0
  where
    go i = case t GV.! fromIntegral i of
             Leaf v       -> v
             Node li k ri -> if goRight k then go ri else go li


-- -- | Lookup an the value of key k
-- lookup   :: Ord k => k -> Tree (Maybe k) (Maybe (k,v)) -> Maybe v
-- lookup q = fmap snd . lookup' (== Just q)

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
