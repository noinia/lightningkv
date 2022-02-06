{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.WithInfty
  ( WithInftyAdapt(..)
  , fmapWithInfty
  , toMaybe
  ) where

import Control.DeepSeq
import Foreign.Storable.Generic
import GHC.Generics

--------------------------------------------------------------------------------

class WithInftyAdapt a where
  data WithInfty a
  destructWithInfty :: WithInfty a -> (a -> b) -> b -> b
  nonInfty :: a -> WithInfty a
  infty :: WithInfty a


fmapWithInfty      :: (WithInftyAdapt a, WithInftyAdapt b) => (a -> b) -> WithInfty a -> WithInfty b
fmapWithInfty f wa = destructWithInfty wa (\x -> nonInfty $ f x)
                                          infty

-- | Convert to a maybe
toMaybe :: WithInftyAdapt a => WithInfty a -> Maybe a
toMaybe n = destructWithInfty n Just Nothing
