module Lightning.Prokob
  ( fromAscListNWith, fromAscListN
  , layoutWith, layout
  , fillWith, fillWith'
  , create

  , Height, Size
  , lg, pow2, size, numLeaves
  ) where


import           Control.Exception (assert)
import           Control.Monad.ST.Strict
import           Control.Monad.Trans.State.Strict ( StateT, evalStateT
                                                  , get, put
                                                  )
import           Data.Semigroup
import           Math.NumberTheory.Logarithms(intLog2')

import qualified Data.Vector as V
import           Lightning.BinTree
import           Lightning.Tree

import qualified Data.Tree.View as TreeView
import qualified Data.Tree as DataTree
--------------------------------------------------------------------------------

type Height = Int

type Size = Int

--------------------------------------------------------------------------------

-- | Given the leaf to value function, a length n, and a list xs of n
-- elements, constructs a BST in VEB layout.
--
-- pre: n=2^h for some h.
fromAscListNWith     :: (b -> a) -> Size -> [b] -> Tree VEB (a, Max a) b
fromAscListNWith f n = layoutWith f (lg n)

-- | Given a length n and a list xs of n elements, constructs a BST in
-- VEB layout.
--
-- pre: n=2^h for some h.
fromAscListN :: Size -> [x] -> Tree VEB (x, Max x) x
fromAscListN = fromAscListNWith id

-- | Given the leaf to value function, a height h, and a list of input
-- elements xs, constructs a BST in VEB layout of the given height.
--
-- pre: length xs = 2^h
layoutWith        :: (b -> a) -> Height -> [b] -> Tree VEB (a, Max a) b
layoutWith f h xs = fillWith f xs $ create h

-- | Lay out 2^h values in a BST in VEB layout.
layout :: Height -> [b] -> Tree VEB (b, Max b) b
layout = layoutWith id

--------------------------------------------------------------------------------

-- | Given a leaf to key function and a list of leaf values,
-- "overwrite" the values in the input tree by those in the input list
-- (in increasing order). Note that this actually constructs a new
-- array in memory sotring this tree.
--
-- This function is useful when the input tree has the right
-- structure/memory layout.
fillWith   :: (d -> c) -> [d] -> Tree l a b -> Tree l (c,Max c) d
fillWith f = fillWith' (\(_,Max lM) _ (_,rM) -> (lM,rM))
                       (\d _ -> d)
                       (\d -> let c = f d in (c,Max c))

type SST s cs = StateT cs (ST s)

-- | More general version of fillWith that allows us to specify how to
-- construct a node, how to create a leaf, and how to lift a leaf into
-- a c.
fillWith'                  :: forall a b c d x l.
                             (c -> a -> c -> c) -- ^ node combinator
                          -> (x -> b -> d)      -- ^ leaf builder
                          -> (d -> c)           -- ^ lift a leaf into c
                          -> [x]
                          -> Tree l a b -> Tree l c d
fillWith' node leaf f xs t = runST (flip evalStateT xs $ biTraverseTreeM node' leaf' lift' t)
  where
    node'         :: Index -> c -> a -> c -> SST s [x] c
    node' _ l a r = pure $ node l a r

    leaf'     :: Index -> b -> SST s [x] d
    leaf' _ b = get >>= \case
                  []      -> error "fillWith: too few elements"
                  (x:xs') -> do put xs'
                                pure $ leaf x b

    lift' :: d -> SST s [x] c
    lift' = pure . f

--------------------------------------------------------------------------------

-- | Create a complete tree in VEB layout of height h
create   :: Height -> Tree VEB () ()
create h = Tree . V.fromListN n . create' $ h
  where
    n = fromIntegral $ size h

type Nodes = [Node () ()]
data PartialTree = PartialTree { startingIndex :: !Index
                               , subTreeNodes  :: Nodes
                               } deriving (Show,Eq)

-- | Implementation of create
create'   :: Height -> Nodes
create' h = case h of
    0 -> [Leaf ()]
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

    numBottoms   = 2 * numLeaves th

    sizeTop      = size th
    sizeBottom   = size bh


-- top should be smaller; leaves should

-- ^ connect the top part with the bottoms by replacing all leaves in top with
-- nodes to the appropriate indices of the bottom roots
connect             :: Nodes -- ^ top
                    -> [PartialTree] -- ^ bottoms
                    -> Nodes
connect top bottoms = f bottoms top
  where
    f bs []               = assert (null bs) []
    f bs (n@Node {}:top') = n : f bs top'
    f bs (Leaf _ : top')  = case bs of
      ( PartialTree il _ : PartialTree ir _ : bs') -> Node il () ir : f bs' top'
      _                                            -> error "connect: too few bottoms!?"

-- | shifts a subtree by startOffset + i*size
shiftBy                       :: Index -> Index -> Nodes -> Word -> PartialTree
shiftBy startOffSet size' t i = PartialTree offSet (fmap f t)
  where
    offSet = startOffSet + i*size'

    f nd = case nd of
             Leaf _     -> nd
             Node l k r -> Node (l+offSet) k (r+offSet)


--------------------------------------------------------------------------------

lg :: Size -> Height
lg = intLog2'

pow2   :: Height -> Word
pow2 h = 2 ^ h

----------------------------------------

-- | size of a complete tree of height h
size :: Height -> Word
size h = pow2 (h + 1) - 1

-- | number of leaves of a complete tree of height h
numLeaves :: Height -> Word
numLeaves = pow2

--------------------------------------------------------------------------------

testTree h = layout h [0..(pow2 h-1)]


printTestTree h = printTree $ testTree h


printTreeOf = printTree . create

printTree :: (Show a, Show b) => Tree l a b -> IO ()
printTree = TreeView.drawTree . convert . toBinTree
  where
    convert (BinLeaf x) = DataTree.Node (show x) []
    convert (BinNode l k r) = DataTree.Node (show k) [convert l, convert r]
