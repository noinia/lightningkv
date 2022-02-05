{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.KeyValueInstances where

import Control.DeepSeq
import Foreign.Storable.Generic
import GHC.Generics
import Thunder.Node
import Thunder.Tree

--------------------------------------------------------------------------------

data instance Node Key Value = Leaf {-# UNPACK #-} !Value
                             | Node {-# UNPACK #-} !Index
                                    {-# UNPACK #-} !Key
                                    {-# UNPACK #-} !Index
                             deriving stock (Show,Eq,Ord,Generic)
                             deriving anyclass (GStorable)
instance NFData (Node Key Value)

instance ConstructNode Key Value where
  leaf = Leaf
  node = Node
instance DestructNode Key Value where
  destructNode n f g = case n of
                         Leaf x     -> f x
                         Node l k r -> g l k r

--------------------------------------------------------------------------------

data instance Node (WithMax Key) Value = LeafWithMax {-# UNPACK #-} !Value
                                       | NodeWithMax {-# UNPACK #-} !Index
                                                     {-# UNPACK #-} !Key
                                                     {-# UNPACK #-} !Key -- ^ the maximum
                                                     {-# UNPACK #-} !Index

instance ConstructNode (WithMax Key) Value where
  leaf = LeafWithMax
  node l km r = destructMax km (\k m -> NodeWithMax l k m r)

instance DestructNode (WithMax Key) Value where
  destructNode n f g = case n of
                         LeafWithMax x       -> f x
                         NodeWithMax l k m r -> g l (withMax k m) r

instance WithMaxAdapt Key where
  data WithMax Key = WithMaxKey {-# UNPACK #-} !Key
                                {-# UNPACK #-} !Key -- the maximum
  withMax = WithMaxKey
  destructMax (WithMaxKey c m) f = f c m


--------------------------------------------------------------------------------
-- * KeyKey Nodes

data instance Node Key Key = LeafKey {-# UNPACK #-} !Key
                           | NodeKey {-# UNPACK #-} !Key
                                     {-# UNPACK #-} !Key
                                     {-# UNPACK #-} !Index
                             deriving stock (Show,Eq,Ord,Generic)
                             deriving anyclass (GStorable)
instance NFData (Node Key Key)

instance ConstructNode Key Key where
  leaf = LeafKey
  node = NodeKey
instance DestructNode Key Key where
  destructNode n f g = case n of
                         LeafKey x     -> f x
                         NodeKey l k r -> g l k r



data instance Node (WithMax Key) Key = LeafWithMaxKey {-# UNPACK #-} !Key
                                     | NodeWithMaxKey {-# UNPACK #-} !Index
                                                      {-# UNPACK #-} !Key
                                                      {-# UNPACK #-} !Key -- ^ the maximum
                                                      {-# UNPACK #-} !Index
                             deriving stock (Show,Eq,Ord,Generic)
                             deriving anyclass (GStorable)

instance ConstructNode (WithMax Key) Key where
  leaf = LeafWithMaxKey
  node l km r = destructMax km (\k m -> NodeWithMaxKey l k m r)

instance DestructNode (WithMax Key) Key where
  destructNode n f g = case n of
                         LeafWithMaxKey x       -> f x
                         NodeWithMaxKey l k m r -> g l (withMax k m) r
