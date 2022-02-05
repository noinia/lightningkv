{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v2 #-}
module Thunder.Prokob
  ( fromAscListNWith, fromAscListN
  , layoutWith, layout
  , fillWith, fillWith'
  , create

  , lg, pow2, size, numLeavesFromHeight

  , padToNextPower
  ) where

import           Control.Exception (assert)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as SV
import           Foreign.Storable.Generic
import           GHC.Generics
import           Math.NumberTheory.Logarithms (intLog2')
import           Thunder.Node
import           Thunder.Tree
import           Thunder.WithInfty

import           Thunder.KeyValueInstances
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | Given the leaf to value function, a length n, and a list xs of n
-- elements, constructs a BST in VEB layout.
fromAscListNWith     ::
                     ( GV.Vector v (Node (WithInfty a) (WithInfty b))
                     , GV.Vector v (Node (WithMax (WithInfty a)) (WithInfty b))
                     , NodeAdapt  (WithMax (WithInfty a)) (WithInfty b)
                     , ConstructNode (WithInfty a) (WithInfty b)
                     , WithMaxAdapt (WithInfty a)
                     )
                     => (b -> a) -> Size -> [b] -> GTree VEB v (WithInfty a) (WithInfty b)
fromAscListNWith f n = let h = lg n in uncurry (layoutWith (fmapWithInfty f)) . padToNextPower h n

-- | Given a length n and a list xs of n elements, constructs a BST in
-- VEB layout.
fromAscListN :: ( GV.Vector v (Node (WithInfty a) (WithInfty a))
                , GV.Vector v (Node (WithMax (WithInfty a)) (WithInfty a))
                , NodeAdapt (WithMax (WithInfty a)) (WithInfty a)
                , ConstructNode (WithInfty a) (WithInfty a)
                , WithMaxAdapt (WithInfty a)
                )
             => Size -> [a] -> GTree VEB v (WithInfty a) (WithInfty a)
fromAscListN = fromAscListNWith id

--------------------------------------------------------------------------------

-- | Given the leaf to value function, a height h, and a list of input
-- elements xs, constructs a BST in VEB layout of the given height.
--
-- pre: length xs = 2^h
layoutWith        :: forall v a b.
                     ( GV.Vector v (Node a b)
                     , GV.Vector v (Node (WithMax a) b)
                     , NodeAdapt (WithMax a) b, ConstructNode a b
                     , WithMaxAdapt a
                     )
                  => (b -> a) -> Height -> [b] -> GTree VEB v a b
layoutWith f h xs = reFillWith f xs $ create @SV.Vector h

-- | Lay out 2^h values in a BST in VEB layout.
layout :: ( DestructNode (WithMax b) b
          , ConstructNode (WithMax b) b
          , ConstructNode b b
          , WithMaxAdapt b
          ) => Height -> [b] -> Tree VEB b b
layout = layoutWith id

--------------------------------------------------------------------------------

-- | Create a complete tree in VEB layout of height h
create   :: GV.Vector v (Node () ()) => Height -> GTree VEB v () ()
create h = Tree . GV.fromListN n . create' $ h
  where
    n = fromIntegral $ size h


data instance Node () () = LeafUnit | NodeUnit {-# UNPACK #-} !Index
                                               {-# UNPACK #-} !Index
                         deriving stock    (Show,Eq,Generic)
                         deriving anyclass (GStorable)

instance ConstructNode () () where
  leaf _ = LeafUnit
  node l _ r = NodeUnit l r
instance DestructNode () () where
  destructNode n f g = case n of
                         LeafUnit     -> f ()
                         NodeUnit l r -> g l () r

type Nodes = [Node () ()]

data PartialTree = PartialTree { startingIndex :: {-# UNPACK #-} !Index
                               , subTreeNodes  :: Nodes
                               } deriving (Show,Eq)

-- | Implementation of create
create'   :: Height -> Nodes
create' h = case h of
    0 -> [LeafUnit]
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
    f bs []       = assert (null bs) []
    f bs (n:top') = destructNode n
           (\_ -> case bs of -- leaf case
                    PartialTree il _ : PartialTree ir _ : bs' -> node il () ir : f bs' top'
                    _                                         -> error "connect: too few bottoms!?"

           )
           (\ _ _ _ -> n : f bs top') -- node case

-- | shifts a subtree by startOffset + i*size
shiftBy                       :: Index -> Index -> Nodes -> Word -> PartialTree
shiftBy startOffSet size' t i = PartialTree offSet (fmap f t)
  where
    offSet = startOffSet + i*size'

    f nd = destructNode nd
             (\_     -> nd)
             (\l k r -> node (l+offSet) k (r+offSet))


--------------------------------------------------------------------------------

lg :: Size -> Height
lg = intLog2'

{-# SPECIALIZE pow2 :: Height -> Word #-}
{-# SPECIALIZE pow2 :: Height -> Int  #-}
pow2   :: Num h => Height -> h
pow2 h = 2 ^ h

----------------------------------------

-- | pad the length to be a power of two
padToNextPower        :: Height -> Size -> [a] -> (Height, [WithInfty a])
padToNextPower h n xs | m == n    = (h, xs')
                      | otherwise = (h+1, xs' <> replicate m Infty)
  where
    m   = pow2 (h+1) - n
    xs' = map Val xs

----------------------------------------

-- | size of a complete tree of height h
size :: Height -> Word
size h = pow2 (h + 1) - 1

-- | number of leaves of a complete tree of height h
numLeavesFromHeight :: Height -> Word
numLeavesFromHeight = pow2

--------------------------------------------------------------------------------

testTree' :: Height -> Tree VEB Key Key
testTree' h = layout h [0..(pow2 h-1)]

testTree    :: ( NodeAdapt (WithMax (WithInfty a)) (WithInfty a)
               , ConstructNode (WithInfty a) (WithInfty a)
               , WithMaxAdapt (WithInfty a)
               )
            => [a] -> Tree VEB (WithInfty a) (WithInfty a)
testTree xs = fromAscListN (length xs) xs

printTestTree :: Height -> IO ()
printTestTree h = printTree $ testTree' h


printTreeOf :: Height -> IO ()
printTreeOf = printTree . create
