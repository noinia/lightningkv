module Key where

import Control.DeepSeq
import Data.Vector.Primitive (Prim)
import Foreign.Storable

--------------------------------------------------------------------------------

-- | Key representing something of type k
newtype Key k = Key Int
              deriving (Show,Eq,Ord,Storable,Prim,NFData) via Int

-- | Make a Key
mkKey :: Enum k => k -> Key k
mkKey = Key . fromEnum

unKey         :: Enum k => Key k -> k
unKey (Key k) = toEnum k

-- type Elem = Int
-- type NodeElem = Int
