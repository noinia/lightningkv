{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}
module Thunder.Node
  ( Node(..)
  , Index
  ) where

import Control.DeepSeq
import Data.Bifunctor
import Foreign.Storable.Generic
import GHC.Generics

--------------------------------------------------------------------------------

type Index  = Word

-- data family Node a b
-- data instance Node Int Int = ILeaf  {-# UNPACK #-} !Int
--                            | INode {-# UNPACK #-} !Index !Int !Index
--                            deriving (Eq,Ord)

-- class CNode a b where
--   getLeaf :: Node a b -> b
--   mkLeaf ::b -> Node a b
--   getNode :: Node a b -> (Index,a,Index)
--   -- FIXME:: use an unboxed tuple? this may screw up performance
--   mkNode :: Index -> a -> Index -> Node a b

-- instance CNode Int Int where
--   getLeaf (ILeaf x) = x
--   getNode (INode l k r) = (l,k,r)
--   mkLeaf = Leaf
--   mkNode = Node


-- pattern Leaf   :: CNode a b => b -> Node a b
-- pattern Leaf b <- (getLeaf -> b)
--   where
--     Leaf = mkLeaf

-- pattern Node       :: CNode a b => Index -> a -> Index -> Node a b
-- pattern Node l k r <- (getNode -> (l,k,r))
--   where
--     Node = mkNode

data Node a b = Leaf {-# UNPACK #-} !b
              | Node {-# UNPACK #-} !Index
                     {-# UNPACK #-} !a
                     {-# UNPACK #-} !Index
              deriving (Show,Eq,Ord,Generic,Generic1,GStorable)

-- data Foo = Foo {-# UNPACK #-} !Int
--                {-# UNPACK #-} !Index
--                {-# UNPACK #-} !Index
--   deriving (Show,Eq)

-- myW = Foo 0 0 3


-- -- test :: Node Int Int
-- -- test = Leaf 5

-- instance (Storable a, Storable b) => Storable (Node a b) where
--   sizeOf _    = 1 + max sizeLeaf sizeNode -- 1 byte extra for the constructor
--     where
--       sizeLeaf = sizeOf (undefined :: b)
--       sizeNode = sizeOf (undefined :: a) + 2* sizeOf (undefined :: Index)

--   alignment _ = lcm (alignment (undefined :: a))
--                     (alignment (undefined :: b))

--   peek ptr = do
--                tag <- peek (castPtr @_ @Word8 ptr)
--                let ptr1 = ptr `plusPtr` alignment @Word8 undefined
--                case tag of
--                    0 -> Leaf <$> peek (castPtr @_ @b ptr1)
--                    1 -> let ptr2 = ptr1 `plusPtr` alignment @Index undefined
--                             ptr3 = ptr2 `plusPtr` alignment @a     undefined
--                         in Node <$> peek (castPtr @_ @Index ptr1)
--                                 <*> peek (castPtr @_ @a     ptr2)
--                                 <*> peek (castPtr @_ @Index ptr3)

--   poke ptr (Leaf x) = do
--                         poke (castPtr @_ @Word8 ptr) 0
--                         let ptr' = ptr `plusPtr` alignment @Word8 undefined
--                         poke (castPtr @_ @b ptr') x
--   poke ptr (Node l k r) = do
--       poke (castPtr ptr :: Ptr Word8) 1
--       let ptr' = ptr `plusPtr` alignment @(Node a b) undefined
--       poke (castPtr ptr' :: Ptr b) x



-- $($(derive [d|
--     instance (Store a, Store b) => Deriving (Store (Node a b))
--     |]))

instance (NFData a, NFData b) => NFData (Node a b) where rnf = rnf1
instance NFData a => NFData1 (Node a)

instance Bifunctor Node where
  bimap f g = \case
    Leaf x     -> Leaf (g x)
    Node l k r -> Node l (f k) r
