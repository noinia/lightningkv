module ThunderKV.Types
  ( Index
  , Height
  , Key(..)
  , Value(..)
  ) where

import           Control.DeepSeq
import           Data.Word
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

type Index = Word64

-- | Measures the height of the tree, leaves have height 0
type Height = Word64

newtype Key = Key Index
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,Enum,NFData)

newtype Value = Value Index
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,Enum,NFData)
