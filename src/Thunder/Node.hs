{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.Node where

import           Control.DeepSeq
import           Control.Exception (assert)
import           Control.Monad (void)
import           Control.Monad.Primitive
import           Control.Monad.Trans.State
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Foreign.Storable.Generic
import           GHC.Generics
import           Math.NumberTheory.Logarithms (intLog2')
import           Prelude hiding (lookup,elem)
import           Thunder.Index

--------------------------------------------------------------------------------

type Key = Int

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
