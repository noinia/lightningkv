{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.Node.Mono
  ( Node(..)
  , Index
  , NodeElem, LeafElem
  ) where

import Control.DeepSeq
import Foreign.Storable.Generic
import GHC.Generics
import Thunder.Tree.Types(Index)

import LeafElem
import NodeElem

--------------------------------------------------------------------------------

data Node = Leaf {-# UNPACK #-} !LeafElem
          | Node {-# UNPACK #-} !Index
                 {-# UNPACK #-} !NodeElem
                 {-# UNPACK #-} !Index
          deriving stock (Show,Eq,Generic)
          deriving anyclass (GStorable,NFData)

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


-- instance Bifunctor Node where
--   bimap f g = \case
--     Leaf x     -> Leaf (g x)
--     Node l k r -> Node l (f k) r
