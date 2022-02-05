module Thunder.Node
  ( Node, NodeAdapt
  , Index
  , ConstructNode(..), DestructNode(..)
  , Key, Value
  -- , Node'(..),
  , bimapNode
  ) where

--------------------------------------------------------------------------------

type Index  = Word
type Key    = Int
type Value  = Int

data family Node k v

class ConstructNode k v where
  leaf :: v -> Node k v
  node :: Index -> k -> Index -> Node k v


class DestructNode k v where
  destructNode :: Node k v
               -> (v -> b)
               -> (Index -> k -> Index -> b)
               -> b


type NodeAdapt k v = (ConstructNode k v, DestructNode k v)

-- data Node a b = Leaf {-# UNPACK #-} !b
--               | Node {-# UNPACK #-} !Index
--                        {-# UNPACK #-} !a
--                        {-# UNPACK #-} !Index
--                deriving stock (Show,Eq,Ord,Generic,Generic1)
--                deriving anyclass (GStorable)


bimapNode       :: (DestructNode k v, ConstructNode k' v')
                => (k -> k') -> (v -> v') -> Node k v -> Node k' v'
bimapNode f g n = destructNode n (\x     -> leaf $ g x)
                                 (\l k r -> node l (f k) r)


    -- \case
    -- Leaf x     -> Leaf (g x)
    -- Node l k r -> Node l (f k) r
