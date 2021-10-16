{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.WithInfty
  ( WithInfty(..)
  , fmapWithInfty
  , toMaybe
  ) where

import Control.DeepSeq
import Foreign.Storable.Generic
import GHC.Generics

--------------------------------------------------------------------------------

data WithInfty a = Val {-# UNPACK #-}!a | Infty
                 deriving stock (Show,Eq,Ord,Generic,Generic1)
                 deriving anyclass (GStorable)

instance (NFData a) => NFData (WithInfty a)
instance NFData1 WithInfty

instance Functor WithInfty where
  fmap = fmapWithInfty


fmapWithInfty   :: (a -> b) -> WithInfty a -> WithInfty b
fmapWithInfty f = \case
    Val x -> Val (f x)
    Infty -> Infty

-- | Convert to a maybe
toMaybe :: WithInfty a -> Maybe a
toMaybe = \case
  Val x -> Just x
  Infty -> Nothing
