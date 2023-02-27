module ThunderKV.Prokob.Clone
  ( structure
  , templates
  , templates'

  , Tree'
  , shiftBy
  , link
  , combine

  , splitSize
  ) where

import qualified Data.Array as Array
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Foldable
import           Data.Traversable
import           ThunderKV.Static.Tree
import           ThunderKV.Types

--------------------------------------------------------------------------------

-- | Creates the structure of a tree
structure   :: Height -> Tree
structure h = fromNonEmpty h $ arr Array.! h
  where
    arr = templates' (h `max` 2)

-- fromAscList :: [(Key,Value)] -> Tree
-- fromAscList =

-- type Templates = Array.Array Height Tree

-- fromAscListWith      :: Templates -> Height -> [(Key,Value)] -> Tree
-- fromAscListWith ts h = temps
--   where
--     h =


type Tree' = NonEmpty FlatNode

-- | Shifts a tree to the right by a given amount
shiftBy       :: Index -> Tree' -> Tree'
shiftBy delta = fmap (shiftRightBy delta)

--------------------------------------------------------------------------------

-- t2 = templates' 2 Array.! 2

-- foo = link (treeSize 2) (treeSize 1) t2

-- traceWith ms x = x

-- traceWith       :: Show a => String -> a -> a
-- traceWith msg x = trace (msg <> "BEGIN " <> show x <> " END") x

--------------------------------------------------------------------------------

imap       :: Array.Ix i => (i -> a -> b) -> Array.Array i a -> Array.Array i b
imap f arr = Array.listArray (Array.bounds arr) . map (uncurry f) $ Array.assocs arr

-- | Given a maximum height, construct templates of the appropriate
-- size for each height.
templates      :: Height -- ^ maximum height, should be at least 2
               -> Array.Array Height Tree
templates maxH = imap fromNonEmpty $ templates' maxH

-- | Given a maximum height, construct templates of the appropriate
-- size for each height.
--
-- this essentially uses a dynamic programing type of approach to
-- compute the trees of the appropriate sizes by repeated cloning.
--
-- pre : the desired max height should be at least 2.
templates'      :: Height -- ^ maximum height, should be at least 2
                -> Array.Array Height Tree'
templates' maxH = arr
  where
    arr = Array.listArray (0,maxH) (t0 : t1 : [ let (ht,hb) = splitSize h
                                                    top     = arr Array.! ht
                                                    bottom  = arr Array.! hb
                                                in combine (ht,top) (hb,bottom)
                                              | h <- [2..maxH]
                                              ]
                                   )
    -- height zero tree
    t0 = NonEmpty.fromList [ FlatLeaf emptyKey emptyValue ]
    -- height one tree. I guess we don't even need this one, since
    -- link will also compute it.
    t1 = NonEmpty.fromList [ FlatNode 1 emptyKey 2
                           , FlatLeaf emptyKey emptyValue
                           , FlatLeaf emptyKey emptyValue
                           ]
    emptyKey = Key 0
    emptyValue = Value 0

-- | Given a height, computes the heights to be used for the top and
-- the bottom trees.
splitSize   :: Height -> (Height,Height)
splitSize h = let ht = h `div` 2
                  hb = if even h then ht - 1 else ht
              in (ht,hb)

-- | Combines the top and bottom tree into a new tree of the sum of
-- their heights.
--
-- we essentially clone the bottom tree (2 * 2 ^ height top = 2 * #top
-- leaves) times, and connect them up. The updated top part of the
-- tree is layed out in memory first.
combine                      :: (Height,Tree') -- ^ top tree and its height
                             -> (Height,Tree') -- ^ bottom tree and its height
                             -> Tree'
combine (ht,top) (hb,bottom) = link nt nb top
                               <> foldMap1 (\i -> shiftBy (nt + i * nb) bottom)
                                           (NonEmpty.fromList [0..2 ^ (ht+1)-1])
  where
    nt = treeSize ht
    nb = treeSize hb

-- | Link a tree of size nt and a tree of size nb
link       :: Size -- ^ size top tree
           -> Size -- ^ size bottom tree
           -> Tree' -> Tree'
link nt nb = snd . mapAccumL f 0
  where
    -- maintains the number of leaves in the top tree
    f i = \case
      FlatLeaf k _ -> (i+1, let li = nt + 2*i*nb
                                ri = nt + (2*i+1)*nb
                            in FlatNode li k ri
                      )
      n            -> (i,n)
