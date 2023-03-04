module ThunderKV.LargeArray
  ( LargeArray
  , fromListN
  , (!)
  , toList
  , imapAccumL, unsafeImapAccumL
  , imapAccumR, unsafeImapAccumR
  , length
  , mapr
  , foldr
  , foldl
  , indicesLR, indicesRL

  , MLargeArray
  , unsafeNew
  , read
  , modify
  , write
  , unsafeFreeze
  , unsafeThaw
  , thaw
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT, get, put, execStateT)
import           Data.Vector.Storable (Vector, MVector)
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as MVector
import           Foreign.Storable
import           GHC.Generics (Generic)
import           Prelude hiding (read, length, foldr, foldl)
import           ThunderKV.Static.Types

--------------------------------------------------------------------------------

newtype LargeArray a = LargeArray (Vector a)
  deriving stock (Show,Read,Eq,Ord,Generic)

instance NFData a => NFData (LargeArray a)

-- | get the underlying vector
toVector                :: LargeArray a -> Vector a
toVector (LargeArray v) = v

-- | Access element i
(!)                :: Storable a => LargeArray a -> Index -> a
(LargeArray v) ! i = v Vector.! i

-- | Construct a large array from a list of elements.
fromListN   :: Storable a => Size -> [a] -> LargeArray a
fromListN n = LargeArray . Vector.fromListN n

toList :: Storable a => LargeArray a -> [a]
toList = Vector.toList . toVector


-- | Get the length of a large array
length :: Storable a => LargeArray a -> Index
length = Vector.length . toVector

-- | Traverse the array right to left accumulating the result
foldr     :: Storable a => (a -> b -> b) -> b -> LargeArray a -> b
foldr f z = Vector.foldr f z . toVector

-- | Traverse the array left to right accumulating the result
foldl     :: Storable a => (b -> a -> b) -> b -> LargeArray a -> b
foldl f z = Vector.foldl f z . toVector

--------------------------------------------------------------------------------

-- | Map over the Array from left to right, while maintaining
-- accumulating some state of type a. Overwrites the array.
unsafeImapAccumL            :: forall a b. (Storable b)
                            => (Index -> a -> b -> (a,b)) -> a -> LargeArray b -> (a, LargeArray b)
unsafeImapAccumL f acc0 arr = runST $ do mArr <- unsafeThaw arr
                                         buildL (read mArr) f acc0 mArr

-- | Map over the Array from left to right, while maintaining
-- accumulating some state of type a.
imapAccumL            :: (Storable b, Storable c)
                      => (Index -> a -> b -> (a,c)) -> a -> LargeArray b -> (a, LargeArray c)
imapAccumL f acc0 arr = runST $ do mArr <- unsafeNew (length arr)
                                   buildL (\i -> pure $ arr ! i) f acc0 mArr

-- | implementation of imapAccumL
buildL                     :: forall a b c s. (Storable b, Storable c)
                           => (Index -> ST s b)
                           -> (Index -> a -> b -> (a,c))
                           -> a
                           -> MLargeArray s c
                           -> ST s (a, LargeArray c)
buildL readArr f acc0 mArr =
    do WithIndex _ acc' <- execStateT go $ WithIndex 0 acc0
       arr' <- unsafeFreeze mArr
       pure (acc',arr')
  where
    go :: StateT (WithIndex a) (ST s) ()
    go = do WithIndex i acc <- get
            b <- lift $ readArr i
            let (acc',c) = f i acc b
            write mArr i c
            put $ WithIndex (succ i) acc'

data WithIndex a = WithIndex {-# UNPACK #-}!Index a


----------------------------------------

-- | map, going from right to left
mapr       :: ( Storable a
              , Storable b
              ) => (a -> b) -> LargeArray a -> LargeArray b
mapr f arr = runST $ do mArr <- unsafeNew (length arr)
                        forM_ (indicesRL arr) $ \i ->
                          write mArr i (f $ arr ! i)
                        unsafeFreeze mArr

indicesLR arr = case length arr of
                  0 -> []
                  n -> [0..n-1]

indicesRL arr = case length arr of
                  0 -> []
                  n -> tail $ [n,n-1..0]


-- | Map over the Array from right to left, while maintaining
-- accumulating some state of type a. Overwrites the array.
unsafeImapAccumR            :: forall a b. (Storable b)
                            => (Index -> a -> b -> (a,b)) -> a -> LargeArray b -> (a, LargeArray b)
unsafeImapAccumR f acc0 arr = runST $ do mArr <- unsafeThaw arr
                                         buildR (read mArr) f acc0 mArr


-- | Map over the Array from right to left, while maintaining
-- accumulating some state of type a.
imapAccumR            :: (Storable b, Storable c)
                      => (Index -> a -> b -> (a,c)) -> a -> LargeArray b -> (a, LargeArray c)
imapAccumR f acc0 arr = runST $ do mArr <- unsafeNew (length arr)
                                   buildR (\i -> pure $ arr ! i) f acc0 mArr

-- | implementation of imapAccumR
buildR                     :: forall a b c s. (Storable b, Storable c)
                           => (Index -> ST s b)
                           -> (Index -> a -> b -> (a,c))
                           -> a
                           -> MLargeArray s c
                           -> ST s (a, LargeArray c)
buildR readArr f acc0 mArr =
    do WithIndex _ acc' <- execStateT go $ WithIndex n acc0
       arr' <- unsafeFreeze mArr
       pure (acc',arr')
  where
    n = length' mArr

    go :: StateT (WithIndex a) (ST s) ()
    go = do WithIndex i acc <- get
            b <- lift $ readArr i
            let (acc',c) = f i acc b
            write mArr i c
            put $ WithIndex (pred i) acc'

-- | get the length of a mutable large array
length' :: Storable a => MLargeArray s a -> Index
length' = MVector.length . toMVector

--------------------------------------------------------------------------------



-- | A Mutable largeArray
newtype MLargeArray s a = MLargeArray { toMVector :: MVector s a }


-- | Read the element at the given indx
read     :: (PrimMonad m, Storable a) => MLargeArray (PrimState m) a -> Index -> m a
read arr = MVector.read (toMVector arr)

-- | apply the function to the element at the given index.
modify     :: (PrimMonad m, Storable a)
           => MLargeArray (PrimState m) a -> (a -> a) -> Index -> m ()
modify arr = MVector.modify (toMVector arr)

-- | Wrtie the value to the given index.
write :: (PrimMonad m, Storable a) => MLargeArray (PrimState m) a -> Index -> a -> m ()
write arr i x = modify arr (const x) i
{-# INLINE write #-}

-- | Unsafe freeze the Mutable large array into a Large Array
unsafeFreeze :: (Storable a, PrimMonad m) => MLargeArray (PrimState m) a -> m (LargeArray a)
unsafeFreeze = fmap LargeArray . Vector.unsafeFreeze . toMVector

-- | Unsafe thaw the Mutable large array into a Large Array
unsafeThaw :: (Storable a, PrimMonad m) => LargeArray a -> m (MLargeArray (PrimState m) a)
unsafeThaw = fmap MLargeArray . Vector.unsafeThaw . toVector

-- | Thaw a lare array
thaw :: (Storable a, PrimMonad m) => LargeArray a -> m (MLargeArray (PrimState m) a)
thaw = fmap MLargeArray . Vector.thaw . toVector


-- copy :: (Storable a, PrimMonad m) => MLargeArray (PrimState m) a -> LargeArray a -> m ()
-- copy = MVector.copy . toMVector

-- | Create a new array of the given size. All elements are left uninitialized.
unsafeNew   :: (PrimMonad m, Storable a) => Index -> m (MLargeArray (PrimState m) a)
unsafeNew n = MLargeArray <$> MVector.unsafeNew n

-- createT :: (Traversable f, Storable a)
--         => (forall s. ST s (f (MLargeArray s a))) -> f (LargeArray a)
-- createT = undefined
