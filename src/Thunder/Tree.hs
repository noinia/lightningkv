{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.Tree where

import           Control.DeepSeq
import           Control.Monad (void)
import           Control.Monad.Primitive
import           Control.Monad.Trans.State
import           Data.Coerce
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Foreign.Storable.Generic
import           GHC.Generics
import           Math.NumberTheory.Logarithms (intLog2')
import           Prelude hiding (lookup,elem)
import           Thunder.Index
import           Thunder.Node

--------------------------------------------------------------------------------


type SubTree layout a = SV.Vector (Node a)
type Size = Int


-- deletions may shrink the acctual size
data Tree layout a = Tree {-# UNPACK #-} !Size
                          (SubTree layout a)
                   deriving stock (Generic)


deriving stock instance NodeProperties (Node a) => Show     (Tree layout a)
deriving stock instance NodeProperties (Node a) => Eq       (Tree layout a)
-- deriving newtype instance NodeProperties (Node a) => Generic  (Tree a)
-- deriving stock instance Generic (Tree a)

deriving anyclass instance NFData (Tree layout a)
-- deriving newtype instance GStorable (Tree a)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------


type Height = Int


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------



-- | pre: the keys are given in ascending order, the tree has the right size
-- size is 2^h
fillUp                     :: forall layout a b.
                              (HasNode a, HasNode b)
                           => [(Key, a)] -> Tree layout b -> Tree layout a
fillUp xs (Tree n subTree) =
    Tree n $ SV.create
           $ flip evalStateT xs
           $ do subTree' <-  SMV.unsafeNew (SV.length subTree)
                traverseBottomUpM subTree subTree' mkKey mkVal
                pure subTree'
  where
    mkKey     :: PrimMonad m => Key -> b -> StateT [(Key,a)] m Key
    mkKey _ _ = gets (fst . head)
    mkVal     :: PrimMonad m => Key -> b -> StateT [(Key,a)] m a
    mkVal _ _ = do v <- gets (snd . head)
                   modify tail
                   pure v

-- |
traverseBottomUpM             :: forall m a b. (HasNode a, HasNode b, PrimMonad m)
                              => SV.Vector (Node a)
                              -> SMV.MVector (PrimState m) (Node b) -- ^ output vector
                              -> (Key -> a -> m Key) -- ^ function to compute the new keys
                              -> (Key -> a -> m b)   -- ^ Function to compute the new value
                              -> m ()
traverseBottomUpM subTree subTree' f g = void $ go root
  where
    -- | We traverse down the input tree, while building the output
    -- tree, and maintaining the subtree Min and Max'es
    go   :: Index (Node a) -> m (Min Key, Max Key)
    go i = caseNode (subTree ! i)
                    leaf'
                    (\l _ _ _ r -> do (mi,Max k) <- go l
                                      (_, ma)    <- go r
                                      node' l mi k ma r
                    )
       where
         -- | our leaf' function constructs the new key and new element.
         -- assuming that the new keys are still monotonically increasing
         leaf'             :: Key -> a -> m (Min Key, Max Key)
         leaf' k a         = do k' <- f k a
                                b  <- g k a
                                SMV.write subTree' (coerce i) (leaf k' b)
                                pure (Min k', Max k')

         -- | given the indices of the "old" left and right subtree, which are also
         -- the right indices for the left and right subtrees of the children,
         -- build the new tree. Compute and assemble the right node.
         node'             :: Index (Node a)
                           -> Min Key -> Key -> Max Key
                           -> Index (Node a)
                           -> m (Min Key, Max Key)
         node' l mi k ma r = (mi,ma)
                             <$ SMV.write subTree' (coerce i)
                                                   (node (coerce l) mi k ma (coerce r))


-- fttomUp         :: forall b. HasNode b => SV.Vector (Node b) -> SubTree VEB b
-- fixBottomUp subTree = SV.create $ do subTree' <- SMV.unsafeNew (SV.length subTree)
--                                      _ <- fixUp subTree'
--                                      pure subTree'
--   where
--     fixUp            :: PrimMonad m => SMV.MVector (PrimState m) (Node b) -> m (Min Key, Max Key)
--     fixUp subTree' = go root
--       where
--         go i = caseNode (subTree ! i)
--                        (\k v       -> leaf' k v)
--                        (\l _ _ _ r -> do (mi,Max k) <- go l
--                                          (_, ma)    <- go r
--                                          node' l mi k ma r
--                        )
--           where
--             leaf' k v         = (Min k, Max k) <$ SMV.write subTree' (coerce i) (leaf k v)
--             node' l mi k ma r = (mi,ma)        <$ SMV.write subTree' (coerce i) (node l mi k ma r)



-- traverseWithKeyInOrder                      :: forall f a b.
--                                                ( Applicative f
--                                                , HasNode a, HasNode b
--                                                )
--                                             => (Key -> a -> f Key)
--                                             -> (Key -> a -> f b) -> Tree a -> f (Tree b)
-- traverseWithKeyInOrder f g (Tree s subTree) = Tree s <$> createT go0
--   where
--     -- go :: forall s. Index (Node a) -> ST s (f (Node b)
--     go0 = do subTree' <- SMV.unsafeNew (SV.length subTree)
--              go subTree' root

--     go subTree' i = caseNode (subTree ! i)
--                              (\k v         -> leaf <$> f k v <*> g k v)
--                              (\l mi k ma r ->
--                              )




elems :: HasNode a => Tree layout a -> [(Key, a)]
elems = toAscList

--
toAscList   :: HasNode a => Tree layout a -> [(Key,a)]
toAscList t = foldLeaves t (\k v -> [(k,v)])
  -- FIXME: this is probably not particularly efficient as is

foldLeaves                        :: forall a layout r. (HasNode a, Semigroup r)
                                  => Tree layout a
                                  -> (Key -> a -> r)
                                  -> r
foldLeaves (Tree _ subTree) leaf' = go root
  where
    go   :: Index (Node a) -> r
    go i = caseNode (subTree ! i)
                    leaf'
                    (\l _ _ _ r -> go l <> go r)





-- | Successor query
lookupGE                    :: forall a layout. HasNode a => Key -> Tree layout a -> Maybe (Key,a)
lookupGE q (Tree _ subTree) = go root
  where
    go   :: Index (Node a) -> Maybe (Key, a)
    go i = caseNode (subTree ! i)
                     (\k v       -> if q <= k then Just (k,v) else Nothing)
                     (\l _ k _ r -> if q <= k then go l else go r)
                   -- this uses the invariant that k = max in left subtree

-- | Prececessor query
lookupLE :: forall a layout. HasNode a => Key -> Tree layout a -> Maybe (Key,a)
lookupLE q (Tree _ subTree) = go root
  where
    go   :: Index (Node a) -> Maybe (Key, a)
    go i = caseNode (subTree ! i)
                     (\k v       -> if k <= q then Just (k,v) else Nothing)
                     (\l _ _ _ r -> let Min k = minKey $ subTree ! r
                                    in if k <= q then go r else go l)

lookup     :: forall a layout. HasNode a => Key -> Tree layout a -> Maybe (Key,a)
lookup q t = case lookupGE q t of
               Nothing                -> Nothing
               Just (k,v) | k == q    -> Just (k,v)
                          | otherwise -> Nothing

elem   :: forall a layout. HasNode a => Key -> Tree layout a -> Bool
elem q = isJust . lookup q


-- | maximum size of this tree.
capacity            :: Storable (Node a) => Tree layout a -> Size
capacity (Tree _ v) = SV.length v

-- | current size of the tree
size            :: Tree layout a -> Size
size (Tree s _) = s


delete :: PrimMonad m => Key -> Tree layout a -> m (Tree layout a)
delete = undefined
-- rebuilds the tree if we halve its size

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

(!)           :: Storable a => SV.Vector a -> Index a -> a
v ! (Index i) = v SV.! i

root :: Index a
root = Index 0

lg :: Size -> Height
lg = intLog2'


-- | number of leaves of a complete tree of height h
numLeavesFromHeight :: Height -> Size
numLeavesFromHeight = pow2

{-# SPECIALIZE pow2 :: Height -> Word #-}
{-# SPECIALIZE pow2 :: Height -> Int  #-}
pow2   :: Num h => Height -> h
pow2 h = 2 ^ h

-- | size of a complete tree of height h
size'   :: Height -> Size
size' h = pow2 (h + 1) - 1
