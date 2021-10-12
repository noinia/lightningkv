module Lightning.Prokob
  ( layout
  , create
  ) where

import           Control.Exception (assert)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Lightning.Tree
import           Lightning.BinTree

import qualified Data.Tree.View as TreeView
import qualified Data.Tree as DataTree
--------------------------------------------------------------------------------

type Height = Word

-- split     :: Int -> Vector a -> [Vector a]
-- split k v = go 0
--   where
--     n = length v
--     go s | s + k < n = slice s k : go (s+k)
--          | otherwise = []


-- pre: n=2^h input elements in xs
layout      :: Height -> Vector b -> (Height, Tree VEB b b)
layout = undefined

-- layout 0 xs = let [x] = xs in (0, [Leaf x])
-- layout h xs = undefined
--   where
--     m = h `div` 2


fillWith               :: ( Semigroup a
                          , GV.Vector v (Node a b)
                          , GV.Vector v (Node k c)
                          )
                       => (d -> k)
                       -> (b -> c -> d)
                       -> (k -> a -> k -> k)
                       -> [c] -> Tree l v a b -> Tree l v k d
fillWith f leaf node xs t@(Tree v) = biTraverseTree node' leaf' t
  where
    n = GV.length v

    leaf' a =
    node



  Tree . V.fromListN n $ go xs (GV.toList v)
  where

    go [] ts = undefined
    go ()



printTreeOf = printTree . create

printTree :: (Show a, Show b) => Tree l a b -> IO ()
printTree = TreeView.drawTree . convert . toBinTree
  where
    convert (BinLeaf x) = DataTree.Node (show x) []
    convert (BinNode l k r) = DataTree.Node (show k) [convert l, convert r]


type Nodes = [Node () ()]
data PartialTree = PartialTree { startingIndex :: !Index
                               , subTreeNodes  :: Nodes
                               } deriving (Show,Eq)

-- | Create a complete tree in VEB layout of height h
create   :: Height -> Tree VEB () ()
create h = Tree . V.fromListN n . create' $ h
  where
    n = fromIntegral $ size h


create'   :: Height -> Nodes
create' h = case h of
    0 -> [Leaf ()]
    _ | h == m*2  -> let top    = create' (m-1)
                         bottom = create' m     in create'' top bottom (m-1) m
      | otherwise -> let top    = create' m     in create'' top top m m
  where
    m = h `div` 2

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

    numBottoms   = 2 * numLeaves th

    sizeTop      = size th
    sizeBottom   = size bh


pow2   :: Height -> Word
pow2 h = 2 ^ h


size :: Height -> Word
size h = pow2 (h + 1) - 1

numLeaves   :: Height -> Word
numLeaves h = pow2 h

-- top should be smaller; leaves should

-- ^ connect the top part with the bottoms by replacing all leaves in top with
-- nodes to the appropriate indices of the bottom roots
connect             :: Nodes -- ^ top
                    -> [PartialTree] -- ^ bottoms
                    -> Nodes
connect top bottoms = f bottoms top
  where
    f bs []                    = assert (null bs) []
    f bs (n@(Node _ _ _):top') = n : f bs top'
    f bs (Leaf _ : top')       = case bs of
      ( PartialTree il _ : PartialTree ir _ : bs') -> Node il () ir : f bs' top'
      _                                            -> error "connect: too few bottoms!?"


shiftBy                       :: Index -> Index -> Nodes -> Word -> PartialTree
shiftBy startOffSet size' t i = PartialTree offSet (fmap f t)
  where
    offSet = startOffSet + i*size'

    f nd = case nd of
             Leaf _     -> nd
             Node l k r -> Node (l+offSet) k (r+offSet)
