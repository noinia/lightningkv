module Thunder.Index where

import           Control.DeepSeq
import           Foreign.Storable.Generic
import           GHC.Generics

--------------------------------------------------------------------------------

newtype Index v = Index Int
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Storable,NFData)
