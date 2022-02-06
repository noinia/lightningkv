{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.Map.KeyMap where

import qualified Data.Foldable as F
import qualified Data.Vector.Storable as SV
import           Prelude hiding (lookup,elem)
import           Thunder.Index
import           Thunder.Tree
import           Thunder.Node
import           Thunder.Prokob

--------------------------------------------------------------------------------


data KeyMap v = KeyMap (Tree VEB (Index v))
                       (SV.Vector v)
              deriving (Show,Eq)


--------------------------------------------------------------------------------

-- |
-- pre: input has length 2^h for some h, keys are in increasing order, No duplicate keys
fromAscList    :: (Foldable f) => f (Key,v) -> KeyMap v
fromAscList xs = fromAscListN (F.length xs) xs

-- |
-- pre: input has length 2^h for some h, keys are in increasing order, No duplicate keys
fromAscListN      :: (Foldable f) => Size -> f (Key,a) -> KeyMap v
fromAscListN n xs = undefined
