module Data.Vector3.Mutable
  ( MVector3(..)
  , IOVector3
  , new
  , replicate
  , replicateM
  , size
  , read
  , write
  , modify
  , imapM_
  , sliceX
  , sliceXY
  ) where

import Prelude hiding (read, replicate)

import Control.DeepSeq         (NFData(..))
import Control.Monad.Primitive

import Data.Typeable (Typeable)

import GHC.Generics

import qualified Data.Vector.Mutable  as V
import           Data.Vector1.Mutable (MVector1(..))
import           Data.Vector2.Mutable (MVector2(..))

type IOVector3 x y = MVector3 x y RealWorld

data MVector3 x y z s a = MVector3
  { num_Ys  :: {-# UNPACK #-} !Int
  , num_Zs  :: {-# UNPACK #-} !Int
  , mvector :: !(V.MVector s a)
  }
  deriving ( Generic, Typeable )

instance NFData (MVector3 x y z s a) where
  rnf (MVector3 _ _ _) = ()

--
new :: PrimMonad m => Int -> Int -> Int -> m (MVector3 x y z (PrimState m) a)
new num_Xs num_Ys num_Zs =
  MVector3 num_Ys num_Zs <$> V.unsafeNew (num_Xs * num_Ys * num_Zs)
{-# INLINE new #-}

replicate :: PrimMonad m => Int -> Int -> Int -> a -> m (MVector3 x y z (PrimState m) a)
replicate num_Xs num_Ys num_Zs a =
  MVector3 num_Ys num_Zs <$> V.replicate (num_Xs * num_Ys * num_Zs) a
{-# INLINE replicate #-}

replicateM :: PrimMonad m => Int -> Int -> Int -> m a -> m (MVector3 x y z (PrimState m) a)
replicateM num_Xs num_Ys num_Zs a =
  MVector3 num_Ys num_Zs <$> V.replicateM (num_Xs * num_Ys * num_Zs) a
{-# INLINE replicateM #-}

--
size :: MVector3 x y z s a -> (Int, Int, Int) -- (xs, ys, zs)
size MVector3{..} =
  (V.length mvector `div` num_Ys `div` num_Zs, num_Ys, num_Zs)
{-# INLINE size #-}

read :: (PrimMonad m, Enum x, Enum y, Enum z)
  => MVector3 x y z (PrimState m) a
  -> x -> y -> z -> m a
read MVector3{..} (fromEnum -> x) (fromEnum -> y) (fromEnum -> z) =
  V.unsafeRead mvector (num_Zs * (num_Ys * x + y) + z)
{-# INLINE read #-}

write :: (PrimMonad m, Enum x, Enum y, Enum z)
  => MVector3 x y z (PrimState m) a
  -> x -> y -> z -> a -> m ()
write MVector3{..} (fromEnum -> x) (fromEnum -> y) (fromEnum -> z) a =
  V.unsafeWrite mvector (num_Zs * (num_Ys * x + y) + z) a
{-# INLINE write #-}

modify :: (PrimMonad m, Enum x, Enum y, Enum z)
  => MVector3 x y z (PrimState m) a
  -> x -> y -> z -> (a -> a) -> m ()
modify MVector3{..} (fromEnum -> x) (fromEnum -> y) (fromEnum -> z) f =
  V.unsafeModify mvector f (num_Zs * (num_Ys * x + y) + z)
{-# INLINE modify #-}

imapM_ :: (PrimMonad m, Enum x, Enum y, Enum z)
  => (a -> x -> y -> z -> m ())
  -> MVector3 x y z (PrimState m) a
  -> m ()
imapM_ f MVector3{..} = go 0
  where
    len = V.length mvector
    go i | i >= len = pure ()
    go i = do
      a <- V.unsafeRead mvector i
      let (xy, z) = i `divMod` num_Zs
          (x, y) = xy `divMod` num_Ys
      f a (toEnum x) (toEnum y) (toEnum z)
      go (i+1)
{-# INLINE imapM_ #-}

-- | Yield just the given YZ-plane of the vector without copying it.
sliceX :: Enum x
  => x -- ^ The plane to slice
  -> MVector3 x y z s a
  -> MVector2 y z s a
sliceX (fromEnum -> x) MVector3{..} =
  MVector2 num_Zs $ V.unsafeSlice (num_Ys * num_Zs * x) (num_Ys * num_Zs) mvector
{-# INLINE sliceX #-}

-- | Yield just the given Z-row of the vector without copying it.
sliceXY :: (Enum x, Enum y)
  => x -- ^ The row to slice
  -> y -- ^ The column to slice
  -> MVector3 x y z s a
  -> MVector1 z s a
sliceXY (fromEnum -> x) (fromEnum -> y) MVector3{..} =
  MVector1 $ V.unsafeSlice (num_Zs * (num_Ys * x + y)) num_Zs mvector
{-# INLINE sliceXY #-}
