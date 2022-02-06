module Thunder.Prokob
  ( VEB
  ) where

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
import           Thunder.Index
import           Thunder.Node
import           Thunder.Tree

--------------------------------------------------------------------------------

data VEB

-- |
-- pre: input has length 2^h for some h, keys are in increasing order, No duplicate keys
fromAscList    :: (Foldable f, HasNode a) => f (Key,a) -> Tree VEB a
fromAscList xs = fromAscListN (F.length xs) xs

-- |
-- pre: input has length 2^h for some h, keys are in increasing order, No duplicate keys
fromAscListN      :: (Foldable f, HasNode a) => Size -> f (Key,a) -> Tree VEB a
fromAscListN n xs = fillUp (F.toList xs) $ create n

--------------------------------------------------------------------------------

-- | creates a tree of size 2^h with keys [0,..,2^h]. The nodes contain all zeros
create   :: Size -> Tree VEB Key
create n = Tree n . SV.fromListN (size' h) $ create' h
  where
    h = lg n


testTree   :: Key -> Tree VEB Key
testTree n = fromAscList . map (\x -> (x,x)) $ [0..(n-1)]


--------------------------------------------------------------------------------





type Nodes = [Node Key]

data PartialTree = PartialTree { startingIndex :: {-# UNPACK #-} !(Index Key)
                               , subTreeNodes  :: Nodes
                               } deriving (Show,Eq)


-- | Implementation of create
create'   :: Height -> Nodes
create' h = case h of
    0 -> [leaf 0 0]
    _ | h == m*2  -> let top    = create' (m-1)
                         bottom = create' m     in create'' top bottom (m-1) m
      | otherwise -> let top    = create' m     in create'' top top m m
  where
    m = h `div` 2

-- | second helper for create.
--
-- main idea is to clone the bottom tree, and then connect the top
-- tree to the clones appropriately.
--
create''                  :: Nodes -- ^ top
                          -> Nodes -- ^ bottom
                          -> Height -- ^ height of the top tree
                          -> Height -- ^ height of the bottom tree
                          -> Nodes
create'' top bottom th bh = top' <> concatMap subTreeNodes bottoms
  where
    top' = connect top bottoms

    bottoms  = map (shiftBy sizeTop sizeBottom bottom) bottoms'
    bottoms' = [0..(numBottoms-1)]

    numBottoms   = 2 * numLeavesFromHeight th

    sizeTop      = size' th
    sizeBottom   = size' bh

-- ^ connect the top part with the bottoms by replacing all leaves in top with
-- nodes to the appropriate indices of the bottom roots
connect             :: Nodes -- ^ top
                    -> [PartialTree] -- ^ bottoms
                    -> Nodes
connect top bottoms = f bottoms top
  where
    f bs []       = assert (null bs) []
    f bs (n:top') = caseNode n
           (\_ _ -> case bs of -- leaf case
                      PartialTree il _ : PartialTree ir _ : bs' ->
                        node (coerce il) (Min 0) 0 (Max 0) (coerce ir) : f bs' top'
                      _                                         ->
                        error "connect: too few bottoms!?"

           )
           (\ _ _ _ _ _ -> n : f bs top') -- node case

  -- I think I should be able to write this uisng fill up. Then
  -- hopefully we can get rid of the lists.


-- | shifts a subtree by startOffset + i*size
shiftBy                       :: Int -- starting offset
                              -> Size -- the size
                              -> Nodes -- the nodes
                              -> Int --
                              -> PartialTree
shiftBy startOffSet size'' t i = PartialTree (Index offSet) (fmap f t)
  where
    offSet = startOffSet + i*size''

    f nd = caseNode nd
             (\_ _                         -> nd)
             (\(Index l) mi k ma (Index r) -> node (Index $ l+offSet) mi k ma (Index $ r+offSet))
