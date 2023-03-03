module ThunderKV.LargeArray
  ( LargeArray
  , fromListN
  , (!)
  , toList
  , imapAccumL
  , length

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
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT, get, put, execStateT)
import           Data.Vector.Storable (Vector, MVector)
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as MVector
import           Foreign.Storable
import           GHC.Generics (Generic)
import           Prelude hiding (read, length)
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



data WithIndex a = WithIndex {-# UNPACK #-}!Index a


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


-- | Get the length of a large array
length :: Storable a => LargeArray a -> Index
length = Vector.length . toVector

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
