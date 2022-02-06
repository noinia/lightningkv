{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.Tree where

import           Control.DeepSeq
import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Foreign.Storable.Generic
import           GHC.Generics
import           Prelude hiding (lookup,elem)
--------------------------------------------------------------------------------

type Key = Int


newtype Index v = Index Int
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Storable,NFData)

deriving newtype instance Storable (Min Key)
deriving newtype instance Storable (Max Key)

type NodeProperties a = (Show a, GStorable a, Eq a, NFData a)

class NodeProperties (Node a) => HasNode a where
  -- ^ invariant: key of the node is max left subtree
  data Node a
  leaf :: Key -> a -> Node a
  node :: Index (Node a) -> Min Key -> Key -> Max Key -> Index (Node a) -> Node a

  caseNode :: Node a -> (Key -> a -> r)
                     -> (Index (Node a) -> Min Key -> Key -> Max Key -> Index (Node a) -> r)
                     -> r

-- | flipped version of caseNode with the Node as last argument.
destructNode               :: HasNode a => (Key -> a -> r)
                           -> (Index (Node a) -> Min Key -> Key -> Max Key -> Index (Node a) -> r)
                           -> Node a
                           -> r
destructNode leaf' node' n = caseNode n leaf' node'

minKey :: HasNode a => Node a -> Min Key
minKey = destructNode (\k _        -> Min k)
                      (\_ mi _ _ _ -> mi)

maxKey :: HasNode a => Node a -> Max Key
maxKey = destructNode (\k _        -> Max k)
                      (\_ _ _ ma _ -> ma)


-- instance Functor Node where
--   fmap f = \case
--     Leaf k v         -> Leaf k (f v)
--     Node l mi k ma r -> Node l mi k ma r

type SubTree layout a = SV.Vector (Node a)
type Size = Int

data VEB

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

data KeyMap v = KeyMap (Tree VEB (Index v))
                       (SV.Vector v)
              deriving (Show,Eq)


--------------------------------------------------------------------------------

-- |
-- pre: input has length 2^h for some h, keys are in increasing order, No duplicate keys
fromAscList :: Foldable f => f (Key,a) -> Tree layout a
fromAscList = undefined

-- |
-- pre: input has length 2^h for some h, keys are in increasing order, No duplicate keys
fromAscListN      :: Foldable f => Size -> f (Key,a) -> Tree layout a
fromAscListN n xs = undefined


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

instance HasNode Int where
  data Node Int = LeafInt {-# UNPACK #-} !Key
                          {-# UNPACK #-} !Int
                | NodeInt {-# UNPACK #-} !(Index (Node Int))
                          {-# UNPACK #-} !(Min Key)
                          {-# UNPACK #-} !Key
                          {-# UNPACK #-} !(Max Key)
                          {-# UNPACK #-} !(Index (Node Int))
                deriving stock (Show,Eq,Generic)
                deriving anyclass (GStorable,NFData)
  leaf = LeafInt
  node = NodeInt
  caseNode n leaf' node' = case n of
                             LeafInt k v         -> leaf' k v
                             NodeInt l mi k ma r -> node' l mi k ma r


instance HasNode (Index v) where
  newtype Node (Index v) = MkNodeIndex (Node Int)
                         deriving newtype (Show,Eq,Generic,GStorable,NFData)
  leaf k (Index v) = coerce $ leaf k v
  node = coerce node
  caseNode (MkNodeIndex n) leaf' node' = caseNode n (coerce leaf') (coerce node')

--------------------------------------------------------------------------------

(!)           :: Storable a => SV.Vector a -> Index a -> a
v ! (Index i) = v SV.! i

root :: Index a
root = Index 0
