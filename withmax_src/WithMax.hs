{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module WithMax
  ( WithMax(..)
  , MaxElem, Elem
  ) where

import MaxElem

import           Control.DeepSeq
import           Foreign.Storable.Generic
import           GHC.Generics

--------------------------------------------------------------------------------

data WithMax = WithMax {-# UNPACK #-} !Elem
                       {-# UNPACK #-} !MaxElem -- ^ the maximum
             deriving stock (Show,Generic)
             deriving anyclass (GStorable,NFData)
