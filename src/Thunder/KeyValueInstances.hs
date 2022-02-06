{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.KeyValueInstances where

import Control.DeepSeq
import Foreign.Storable.Generic
import GHC.Generics
import Thunder.Node
import Thunder.Tree
import Thunder.WithInfty

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

-- data instance Node Key Key = LeafKey {-# UNPACK #-} !Key
--                            | NodeKey {-# UNPACK #-} !Key
--                                      {-# UNPACK #-} !Key
--                                      {-# UNPACK #-} !Index
--                              deriving stock (Show,Eq,Ord,Generic)
--                              deriving anyclass (GStorable)
-- instance NFData (Node Key Key)

-- instance ConstructNode Key Key where
--   leaf = LeafKey
--   node = NodeKey
-- instance DestructNode Key Key where
--   destructNode n f g = case n of
--                          LeafKey x     -> f x
--                          NodeKey l k r -> g l k r



-- data instance Node (WithMax Key) Key = LeafWithMaxKey {-# UNPACK #-} !Key
--                                      | NodeWithMaxKey {-# UNPACK #-} !Index
--                                                       {-# UNPACK #-} !Key
--                                                       {-# UNPACK #-} !Key -- ^ the maximum
--                                                       {-# UNPACK #-} !Index
--                              deriving stock (Show,Eq,Ord,Generic)
--                              deriving anyclass (GStorable)

-- instance ConstructNode (WithMax Key) Key where
--   leaf = LeafWithMaxKey
--   node l km r = destructMax km (\k m -> NodeWithMaxKey l k m r)

-- instance DestructNode (WithMax Key) Key where
--   destructNode n f g = case n of
--                          LeafWithMaxKey x       -> f x
--                          NodeWithMaxKey l k m r -> g l (withMax k m) r

-- --------------------------------------------------------------------------------


instance WithInftyAdapt Key where
  data WithInfty Key = WithInftyKey {-# UNPACK #-} !Key | InftyKey
                     deriving (Show,Eq,Ord)
  destructWithInfty wi f g = case wi of
                               WithInftyKey k -> f k
                               InftyKey       -> g
  nonInfty = WithInftyKey
  infty = InftyKey

-- instance WithInftyAdapt Value where
--   data WithInfty Key = WithInftyValue {-# UNPACK #-} !Value | InftyValue
--                      deriving (Show,Eq,Ord)
--   destructWithInfty wi f g = case wi of
--                                WithInftyValue k -> f k
--                                InftyValue       -> g
--   nonInfty = WithInftyValue
--   infty = InftyValue


data instance Node (WithInfty Key) (WithInfty Value) =
    LeafVal  {-# UNPACK #-} !Value
  | LeafInfty
  | NodeVal {-# UNPACK #-} !Index
            {-# UNPACK #-} !Key
            {-# UNPACK #-} !Index
  | NodeInfty {-# UNPACK #-} !Index
              {-# UNPACK #-} !Index


instance ConstructNode (WithInfty Key) (WithInfty Value) where
  leaf mv     = destructWithInfty mv LeafVal LeafInfty
  node l mk r = destructWithInfty mk
                  (\k -> NodeVal l k r)
                  (NodeInfty l r)

instance DestructNode (WithInfty Key) (WithInfty Value) where
  destructNode n f g = case n of
                         LeafVal x     -> f (nonInfty x)
                         LeafInfty     -> f infty
                         NodeVal l k r -> g l (nonInfty k) r
                         NodeInfty l r -> g l infty r

--------------------------------------------------------------------------------

instance WithMaxAdapt (WithMax (WithInfy Key)) where
  data WithMax Key = ValVal {-# UNPACK #-} !Key
                            {-# UNPACK #-} !Key -- the maximum
                   | ValInfty {-# UNPACK #-} !Key -- maximum is infty
                   | InftyInfty  -- if the key is infty, so is the max
                   deriving (Show,Eq)

  withMax = WithMaxKey
  destructMax (WithMaxKey c m) f = f c m



data instance Node (WithMax (WithInfty Key)) (WithInfty Value) =
    LeafVal'  {-# UNPACK #-} !Value
  | LeafInfty'
  | NodeVal' {-# UNPACK #-} !Index
             {-# UNPACK #-} !Key
             {-# UNPACK #-} !Key
             {-# UNPACK #-} !Index
  | NodeInfty' {-# UNPACK #-} !Index
               {-# UNPACK #-} !Index


instance ConstructNode (WithInfty Key) (WithInfty Value) where
  leaf mv     = destructWithInfty mv LeafVal LeafInfty
  node l mk r = destructWithInfty mk
                  (\k -> NodeVal l k r)
                  (NodeInfty l r)

instance DestructNode (WithInfty Key) (WithInfty Value) where
  destructNode n f g = case n of
                         LeafVal x     -> f (nonInfty x)
                         LeafInfty     -> f infty
                         NodeVal l k r -> g l (nonInfty k) r
                         NodeInfty l r -> g l infty r
