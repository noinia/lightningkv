{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.Prokob
  ( fromAscListNWith, fromAscListN
  , layoutWith, layout
  , fillWith, fillWith'
  , create

  , Height, Size
  , lg, pow2, size, numLeaves


  , printTree
  ) where



import           Control.Exception (assert)
import           Control.Monad.ST.Strict
import           Control.Monad.Trans.State.Strict ( StateT, evalStateT
                                                  , get, put
                                                  )
import qualified Data.Tree as DataTree
import qualified Data.Tree.View as TreeView
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as SV
import           Foreign.Storable.Generic
import           GHC.Generics
import           Math.NumberTheory.Logarithms (intLog2')
import           Thunder.BinTree
import           Thunder.Node
import           Thunder.Tree
import           Thunder.WithInfty

--------------------------------------------------------------------------------

type Height = Int

type Size = Int

--------------------------------------------------------------------------------

-- | Given the leaf to value function, a length n, and a list xs of n
-- elements, constructs a BST in VEB layout.
fromAscListNWith     ::
                     ( GV.Vector v (Node (WithInfty a) (WithInfty b))
                     , GV.Vector v (Node (WithMax (WithInfty a)) (WithInfty b))
                     )
                     => (b -> a) -> Size -> [b] -> GTree VEB v (WithInfty a) (WithInfty b)
fromAscListNWith f n = let h = lg n in uncurry (layoutWith (fmap f)) . padToNextPower h n

-- | Given a length n and a list xs of n elements, constructs a BST in
-- VEB layout.
fromAscListN :: ( GV.Vector v (Node (WithInfty a) (WithInfty a))
                , GV.Vector v (Node (WithMax (WithInfty a)) (WithInfty a))
                )
             => Size -> [a] -> GTree VEB v (WithInfty a) (WithInfty a)
fromAscListN = fromAscListNWith id


-- | pad the length to be a power of two
padToNextPower        :: Height -> Size -> [a] -> (Height, [WithInfty a])
padToNextPower h n xs | m == 0    = (h, xs')
                      | otherwise = (h+1, xs' <> replicate m Infty)
  where
    m   = n - pow2 h
    xs' = map Val xs


--------------------------------------------------------------------------------

-- | Given the leaf to value function, a height h, and a list of input
-- elements xs, constructs a BST in VEB layout of the given height.
--
-- pre: length xs = 2^h
layoutWith        :: forall v a b.
                     ( GV.Vector v (Node a b)
                     , GV.Vector v (Node (WithMax a) b)
                     )
                  => (b -> a) -> Height -> [b] -> GTree VEB v a b
layoutWith f h xs = bimapTree (\(WithMax x _) -> x) id
                  . fillWith f xs
                  $ create @SV.Vector h

-- | Lay out 2^h values in a BST in VEB layout.
layout :: Height -> [b] -> Tree VEB b b
layout = layoutWith id

--------------------------------------------------------------------------------

-- | Given a leaf to key function and a list of leaf values,
-- "overwrite" the values in the input tree by those in the input list
-- (in increasing order). Note that this actually constructs a new
-- array in memory sotring this tree.
--
-- This function is useful when the input tree has the right
-- structure/memory layout.
fillWith   :: ( GV.Vector v (Node a b)
              , GV.Vector w (Node (WithMax c) d)
              )
           => (d -> c) -> [d] -> GTree l v a b -> GTree l w (WithMax c) d
fillWith f = fillWith' (\(WithMax _ lM) _ (WithMax _ rM) -> WithMax lM rM)
                       (\d _ -> d)
                       (\d -> let c = f d in WithMax c c)

data WithMax c = WithMax {-# UNBOX #-} !c
                         {-# UNBOX #-} !c -- the maximum
               deriving stock (Show,Eq,Ord,Generic)
               deriving anyclass (GStorable)

type SST s cs = StateT cs (ST s)

-- | More general version of fillWith that allows us to specify how to
-- construct a node, how to create a leaf, and how to lift a leaf into
-- a c.
fillWith'                  :: forall v w a b c d x l.
                           ( GV.Vector v (Node a b)
                           , GV.Vector w (Node c d)
                           )
                          => (c -> a -> c -> c) -- ^ node combinator
                          -> (x -> b -> d)      -- ^ leaf builder
                          -> (d -> c)           -- ^ lift a leaf into c
                          -> [x]
                          -> GTree l v a b -> GTree l w c d
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
create   :: GV.Vector v (Node () ()) => Height -> GTree VEB v () ()
create h = Tree . GV.fromListN n . create' $ h
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

{-# SPECIALIZE pow2 :: Height -> Word #-}
{-# SPECIALIZE pow2 :: Height -> Int  #-}
pow2   :: Num h => Height -> h
pow2 h = 2 ^ h

----------------------------------------

-- | size of a complete tree of height h
size :: Height -> Word
size h = pow2 (h + 1) - 1

-- | number of leaves of a complete tree of height h
numLeaves :: Height -> Word
numLeaves = pow2

--------------------------------------------------------------------------------

testTree' :: Height -> Tree VEB Word Word
testTree' h = layout h [0..(pow2 h-1)]

testTree    :: [a] -> Tree VEB (WithInfty a) (WithInfty a)
testTree xs = fromAscListN (length xs) xs


printTestTree :: Height -> IO ()
printTestTree h = printTree $ testTree' h


printTreeOf :: Height -> IO ()
printTreeOf = printTree . create

printTree :: (Show a, Show b) => Tree l a b -> IO ()
printTree = TreeView.drawTree . convert . toBinTree
  where
    convert (BinLeaf x) = DataTree.Node (show x) []
    convert (BinNode l k r) = DataTree.Node (show k) [convert l, convert r]
