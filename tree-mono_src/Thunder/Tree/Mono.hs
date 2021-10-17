{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
module Thunder.Tree.Mono
  ( Tree(..)
  , toBinTree
  , numLeaves, numNodes

  , leaves
  , searchLeafR


  , Node(..), LeafElem, NodeElem
  -- , WithMax

  -- , reFillWith
  -- , fillWith, fillWith'


  , printTree
  ) where

import           Control.DeepSeq
-- import           Control.Monad.Primitive (PrimMonad)
-- import           Control.Monad.ST.Strict
-- import           Control.Monad.Trans.State.Strict ( StateT, evalStateT
--                                                   , get, put
--                                                   )
-- import           Data.Bifoldable
-- import           Data.Bifunctor
-- import           Data.Functor.Identity
import qualified Data.Tree as DataTree
import qualified Data.Tree.View as TreeView
import qualified Data.Vector.Storable as SV
-- import qualified Data.Vector.Generic.Mutable as GVM
-- import           Foreign.Storable.Generic
import           GHC.Generics
-- -- import           Data.Word (Word8)

-- import           Data.Store
-- import           Data.Store.TH
import           Thunder.BinTree (BinTree(..))
import           Thunder.Node.Mono
import           Thunder.Tree.Types
-- -- import           Foreign.Storable
-- -- import           Foreign.Ptr

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | A Tree whose leaves store values of type b, whose internal nodes
-- store values of type a. The tree is stored in an Vector v using
-- layout l.
newtype Tree (l :: Layout) = Tree (SV.Vector Node)
  deriving (Generic, Show, Eq)
  deriving anyclass (NFData)

-- instance NFData (v (Node a b)) => NFData (GTree l v a b)

-- instance Functor v => Bifunctor (GTree l v) where
--   bimap f g (Tree v) = Tree $ fmap (bimap f g) v





-- | Convert into a BinTree
toBinTree :: Tree l -> BinTree NodeElem LeafElem
toBinTree (Tree t) = go 0
  where
    go i = case t SV.! fromIntegral i of
             Leaf x     -> BinLeaf x
             Node l k r -> BinNode (go l) k (go r)

-- | Get the number of nodes in the tree
numNodes          :: Tree l -> Int
numNodes (Tree v) = SV.length v

-- | get the capacity, i.e. the number of leaves of the tree
numLeaves   :: Tree l -> Int
numLeaves t = (numNodes t + 1) `div` 2




-- instance Bifoldable (Tree l) where
--   bifoldMap f = bifoldTree (\l k r -> l <> f k <> r)





--------------------------------------------------------------------------------
-- * Traversals

-- | Get the values in the leaves in ascending order.
leaves          :: Tree l -> SV.Vector LeafElem
leaves (Tree t) = SV.mapMaybe (\case
                                  Leaf x -> Just x
                                  _      -> Nothing) t

--------------------------------------------------------------------------------
-- * Querying

-- | Binary-search on a tree. the predicate indicates if we should go right
searchLeafR                  :: (NodeElem -> Bool) -> Tree l -> LeafElem
searchLeafR goRight (Tree t) = go 0
  where
    go i = case t SV.! fromIntegral i of
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


-- --------------------------------------------------------------------------------
-- -- * Filling Binary Search Trees

-- -- | Given a leaf to value function, an ascending list of elements xs,
-- -- and a tree t, create a BST by overwriting all values in the BST.
-- --
-- -- pre: length xs >= capacity t
-- reFillWith      :: ( GV.Vector v (Node a b)
--                    , GV.Vector w (Node (WithMax c) d)
--                    , GV.Vector w (Node c d)
--                    )
--                 => (d -> c) -> [d] -> Tree l a b -> GTree l w c d
-- reFillWith f xs = bimapTree (\(WithMax x _) -> x) id . fillWith f xs

-- -- | Given a leaf to key function and a list of leaf values,
-- -- "overwrite" the values in the input tree by those in the input list
-- -- (in increasing order). Note that this actually constructs a new
-- -- array in memory sotring this tree.
-- --
-- -- This function is useful when the input tree has the right
-- -- structure/memory layout.
-- fillWith   :: ( GV.Vector v (Node a b)
--               , GV.Vector w (Node (WithMax c) d)
--               )
--            => (d -> c) -> [d] -> Tree l a b -> GTree l w (WithMax c) d
-- fillWith f = fillWith' (\(WithMax _ lM) _ (WithMax _ rM) -> WithMax lM rM)
--                        (\d _ -> d)
--                        (\d -> let c = f d in WithMax c c)

-- data WithMax c = WithMax {-# UNBOX #-} !c
--                          {-# UNBOX #-} !c -- the maximum
--                deriving stock (Show,Eq,Ord,Generic)
--                deriving anyclass (GStorable)

-- type SST s cs = StateT cs (ST s)

-- -- | More general version of fillWith that allows us to specify how to
-- -- construct a node, how to create a leaf, and how to lift a leaf into
-- -- a c.
-- fillWith'                  :: forall v w a b c d x l.
--                            ( GV.Vector v (Node a b)
--                            , GV.Vector w (Node c d)
--                            )
--                           => (c -> a -> c -> c) -- ^ node combinator
--                           -> (x -> b -> d)      -- ^ leaf builder
--                           -> (d -> c)           -- ^ lift a leaf into c
--                           -> [x]
--                           -> Tree l a b -> GTree l w c d
-- fillWith' node leaf f xs t = runST (flip evalStateT xs $ biTraverseTreeM node' leaf' lift' t)
--   where
--     node'         :: Index -> c -> a -> c -> SST s [x] c
--     node' _ l a r = pure $ node l a r

--     leaf'     :: Index -> b -> SST s [x] d
--     leaf' _ b = get >>= \case
--                   []      -> error "fillWith: too few elements"
--                   (x:xs') -> do put xs'
--                                 pure $ leaf x b

--     lift' :: d -> SST s [x] c
--     lift' = pure . f

--------------------------------------------------------------------------------
-- * Debugging

printTree :: Tree l -> IO ()
printTree = TreeView.drawTree . convert . toBinTree
  where
    convert (BinLeaf x) = DataTree.Node (show x) []
    convert (BinNode l k r) = DataTree.Node (show k) [convert l, convert r]

--------------------------------------------------------------------------------
