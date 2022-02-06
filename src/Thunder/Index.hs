module Thunder.Index where

import           Control.DeepSeq
import           Control.Exception (assert)
import           Control.Monad (void)
import           Control.Monad.Primitive
import           Control.Monad.Trans.State
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Foreign.Storable.Generic
import           GHC.Generics
import           Math.NumberTheory.Logarithms (intLog2')
import           Prelude hiding (lookup,elem)

--------------------------------------------------------------------------------

newtype Index v = Index Int
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Storable,NFData)
