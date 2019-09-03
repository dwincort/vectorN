module Data.Vector2.Mutable
  ( MVector2(..)
  , IOVector2
  , new
  , replicate
  , replicateM
  , size
  , read
  , write
  , modify
  , imapM_
  , sliceX
  ) where

import Prelude hiding (read, replicate)

import Control.DeepSeq         (NFData(..))
import Control.Monad.Primitive

import Data.Typeable (Typeable)

import GHC.Generics

import qualified Data.Vector.Mutable  as V
import           Data.Vector1.Mutable (MVector1(..))

type IOVector2 x y = MVector2 x y RealWorld

data MVector2 x y s a = MVector2
  { num_Ys  :: {-# UNPACK #-} !Int
  , mvector :: !(V.MVector s a)
  }
  deriving ( Generic, Typeable )

instance NFData (MVector2 x y s a) where
  rnf (MVector2 _ _) = ()

--
new :: PrimMonad m => Int -> Int -> m (MVector2 x y (PrimState m) a)
new num_Xs num_Ys =
  MVector2 num_Ys <$> V.unsafeNew (num_Xs * num_Ys)
{-# INLINE new #-}

replicate :: PrimMonad m => Int -> Int -> a -> m (MVector2 x y (PrimState m) a)
replicate num_Xs num_Ys a =
  MVector2 num_Ys <$> V.replicate (num_Xs * num_Ys) a
{-# INLINE replicate #-}

replicateM :: PrimMonad m => Int -> Int -> m a -> m (MVector2 x y (PrimState m) a)
replicateM num_Xs num_Ys a =
  MVector2 num_Ys <$> V.replicateM (num_Xs * num_Ys) a
{-# INLINE replicateM #-}

--
size :: MVector2 x y s a -> (Int, Int) -- (xs, ys)
size MVector2{..} =
  (V.length mvector `div` num_Ys, num_Ys)
{-# INLINE size #-}

read :: (PrimMonad m, Enum x, Enum y)
  => MVector2 x y (PrimState m) a
  -> x -> y -> m a
read MVector2{..} (fromEnum -> x) (fromEnum -> y) =
  V.unsafeRead mvector (num_Ys * x + y)
{-# INLINE read #-}

write :: (PrimMonad m, Enum x, Enum y)
  => MVector2 x y (PrimState m) a
  -> x -> y -> a -> m ()
write MVector2{..} (fromEnum -> x) (fromEnum -> y) a =
  V.unsafeWrite mvector (num_Ys * x + y) a
{-# INLINE write #-}

modify :: (PrimMonad m, Enum x, Enum y)
  => MVector2 x y (PrimState m) a
  -> x -> y -> (a -> a) -> m ()
modify MVector2{..} (fromEnum -> x) (fromEnum -> y) f =
  V.unsafeModify mvector f (num_Ys * x + y)
{-# INLINE modify #-}

imapM_ :: (PrimMonad m, Enum x, Enum y)
  => (a -> x -> y -> m ())
  -> MVector2 x y (PrimState m) a
  -> m ()
imapM_ f MVector2{..} = go 0
  where
    len = V.length mvector
    go i | i >= len = pure ()
    go i = do
      a <- V.unsafeRead mvector i
      let (x, y) = i `divMod` num_Ys
      f a (toEnum x) (toEnum y)
      go (i+1)
{-# INLINE imapM_ #-}

-- | Yield just the given row of the vector without copying it.
sliceX :: Enum x
  => x -- ^ The row to slice
  -> MVector2 x y s a
  -> MVector1 y s a
sliceX (fromEnum -> x) MVector2{..} =
  MVector1 $ V.unsafeSlice (num_Ys * x) num_Ys mvector
{-# INLINE sliceX #-}
