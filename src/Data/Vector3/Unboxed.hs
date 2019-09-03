{-# LANGUAGE DeriveAnyClass #-}
module Data.Vector3.Unboxed
  ( Vector3(..)
  , V.Unbox
  , replicate
  , replicateM
  , generateM
  , create
  , size
  , read
  , mapM
  , mapM_
  , imapM
  , imapM_
  , sliceX
  , sliceXY
  , sliceZ
  , unsafeFreeze
  , unsafeThaw
  , freeze
  , thaw
  ) where

import Prelude hiding (mapM, mapM_, read, replicate, zipWith)

import Control.DeepSeq         (NFData)
import Control.Monad.Primitive
import Control.Monad.ST        (ST)

import Data.Typeable (Typeable)

import GHC.Generics

import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import           Data.Vector1.Unboxed         (Vector1(..))
import           Data.Vector2.Unboxed         (Vector2(..))
import           Data.Vector3.Unboxed.Mutable (MVector3(..))

data Vector3 x y z a = Vector3
  { num_Ys :: {-# UNPACK #-} !Int
  , num_Zs :: {-# UNPACK #-} !Int
  , vector :: !(V.Vector a)
  }
  deriving ( Generic, Show, Typeable, NFData, Eq )

--
replicate :: V.Unbox a => Int -> Int -> Int -> a -> Vector3 x y z a
replicate num_Xs num_Ys num_Zs a =
  Vector3 num_Ys num_Zs (V.replicate (num_Xs * num_Ys * num_Zs) a)
{-# INLINE replicate #-}

replicateM :: (Monad m, V.Unbox a) => Int -> Int -> Int -> m a -> m (Vector3 x y z a)
replicateM num_Xs num_Ys num_Zs a =
  Vector3 num_Ys num_Zs <$> V.replicateM (num_Xs * num_Ys * num_Zs) a
{-# INLINE replicateM #-}

generateM :: (Monad m, V.Unbox a, Enum x, Enum y, Enum z)
  => Int -> Int -> Int -> (x -> y -> z -> m a) -> m (Vector3 x y z a)
generateM num_Xs num_Ys num_Zs f =
  Vector3 num_Ys num_Zs <$> V.generateM (num_Xs * num_Ys * num_Zs) g
  where
    g i =
      let (xy, z) = i `divMod` num_Zs
          (x, y) = xy `divMod` num_Ys
      in f (toEnum x) (toEnum y) (toEnum z)
{-# INLINE generateM #-}

create :: forall x y z a. V.Unbox a => (forall s. ST s (MVector3 x y z s a)) -> Vector3 x y z a
create v3m = Vector3{..}
  where
    action :: ST s ((Int, Int), MV.MVector s a)
    action = do
      MVector3{..} <- v3m
      pure ((num_Ys, num_Zs), mvector)
    ((num_Ys, num_Zs), vector) = V.createT action
{-# INLINE create #-}

--
size :: V.Unbox a => Vector3 x y z a -> (Int, Int, Int) -- (xs, ys, zs)
size Vector3{..} =
  (V.length vector `div` num_Ys `div` num_Zs, num_Ys, num_Zs)
{-# INLINE size #-}

read :: (V.Unbox a, Enum x, Enum y, Enum z)
  => Vector3 x y z a -> x -> y -> z -> a
read Vector3{..} (fromEnum -> x) (fromEnum -> y) (fromEnum -> z) =
  V.unsafeIndex vector (num_Zs * (num_Ys * x + y) + z)
{-# INLINE read #-}

mapM_ :: (Monad m, V.Unbox a) => (a -> m ()) -> Vector3 x y z a -> m ()
mapM_ f Vector3{..} = V.mapM_ f vector
{-# INLINE mapM_ #-}

mapM :: (Monad m, V.Unbox a, V.Unbox b) => (a -> m b) -> Vector3 x y z a -> m (Vector3 x y z b)
mapM f Vector3{..} =
  Vector3 num_Ys num_Zs <$> V.mapM f vector
{-# INLINE mapM #-}

imapM :: (Monad m, V.Unbox a, V.Unbox b, Enum x, Enum y, Enum z)
  => (x -> y -> z -> a -> m b)
  -> Vector3 x y z a -> m (Vector3 x y z b)
imapM f Vector3{..} =
  Vector3 num_Ys num_Zs <$> V.imapM g vector
  where
    g i =
      let (xy, z) = i `divMod` num_Zs
          (x, y) = xy `divMod` num_Ys
      in f (toEnum x) (toEnum y) (toEnum z)

imapM_ :: (Monad m, V.Unbox a, Enum x, Enum y, Enum z)
  => (a -> x -> y -> z -> m ())
  -> Vector3 x y z a -> m ()
imapM_ f Vector3{..} = V.imapM_ g vector
  where
    g i a =
      let (xy, z) = i `divMod` num_Zs
          (x, y) = xy `divMod` num_Ys
      in f a (toEnum x) (toEnum y) (toEnum z)
{-# INLINE imapM_ #-}

-- | Yield just the given YZ-plane of the vector without copying it.
sliceX :: (Enum x, V.Unbox a)
  => x -- ^ The plane to slice
  -> Vector3 x y z a
  -> Vector2 y z a
sliceX (fromEnum -> x) Vector3{..} =
  Vector2 num_Zs $ V.unsafeSlice (num_Ys * num_Zs * x) (num_Ys * num_Zs) vector
{-# INLINE sliceX #-}

-- | Yield just the given Z-row of the vector without copying it.
sliceXY :: (Enum x, Enum y, V.Unbox a)
  => x -- ^ The row to slice
  -> y -- ^ The column to slice
  -> Vector3 x y z a
  -> Vector1 z a
sliceXY (fromEnum -> x) (fromEnum -> y) Vector3{..} =
  Vector1 $ V.unsafeSlice (num_Zs * (num_Ys * x + y)) num_Zs vector
{-# INLINE sliceXY #-}

-- | Yield just the given XY-plane.
-- This _will_ create a new vector.
-- TODO: Create a newtype vector that prevents this from needing to copy.
sliceZ :: (Enum z, V.Unbox a)
  => z -- ^ The z-row to slice
  -> Vector3 x y z a
  -> Vector2 x y a
sliceZ (fromEnum -> z) Vector3{..} =
  Vector2 num_Ys $ V.generate (V.length vector `div` num_Zs) (\i -> vector V.! (num_Zs * i + z))

unsafeFreeze :: (PrimMonad m, V.Unbox a)
  => MVector3 x y z (PrimState m) a
  -> m (Vector3 x y z a)
unsafeFreeze MVector3{..} = Vector3 num_Ys num_Zs <$> V.unsafeFreeze mvector
{-# INLINE unsafeFreeze #-}

unsafeThaw :: (PrimMonad m, V.Unbox a)
  => Vector3 x y z a
  -> m (MVector3 x y z (PrimState m) a)
unsafeThaw Vector3{..} = MVector3 num_Ys num_Zs <$> V.unsafeThaw vector
{-# INLINE unsafeThaw #-}

freeze :: (PrimMonad m, V.Unbox a)
  => MVector3 x y z (PrimState m) a
  -> m (Vector3 x y z a)
freeze MVector3{..} = Vector3 num_Ys num_Zs <$> V.freeze mvector
{-# INLINE freeze #-}

thaw :: (PrimMonad m, V.Unbox a)
  => Vector3 x y z a
  -> m (MVector3 x y z (PrimState m) a)
thaw Vector3{..} = MVector3 num_Ys num_Zs <$> V.thaw vector
{-# INLINE thaw #-}
