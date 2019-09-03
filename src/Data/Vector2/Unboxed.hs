{-# LANGUAGE DeriveAnyClass #-}
module Data.Vector2.Unboxed
  ( Vector2(..)
  , V.Unbox
  , replicate
  , replicateM
  , generateM
  , create
  , fromList
  , size
  , read
  , mapM
  , mapM_
  , imapM
  , imapM_
  , sliceX
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
import           Data.Vector2.Unboxed.Mutable (MVector2(..))

data Vector2 x y a = Vector2
  { num_Ys :: {-# UNPACK #-} !Int
  , vector :: !(V.Vector a)
  }
  deriving ( Generic, Show, Typeable, NFData, Eq )

--
replicate :: V.Unbox a => Int -> Int -> a -> Vector2 x y a
replicate num_Xs num_Ys a =
  Vector2 num_Ys (V.replicate (num_Xs * num_Ys) a)
{-# INLINE replicate #-}

replicateM :: (Monad m, V.Unbox a) => Int -> Int -> m a -> m (Vector2 x y a)
replicateM num_Xs num_Ys a =
  Vector2 num_Ys <$> V.replicateM (num_Xs * num_Ys) a
{-# INLINE replicateM #-}

generateM :: (Monad m, V.Unbox a, Enum x, Enum y)
  => Int -> Int -> (x -> y -> m a) -> m (Vector2 x y a)
generateM num_Xs num_Ys f =
  Vector2 num_Ys <$> V.generateM (num_Xs * num_Ys) g
  where
    g i =
      let (x, y) = i `divMod` num_Ys
      in f (toEnum x) (toEnum y)
{-# INLINE generateM #-}

create :: forall x y a. V.Unbox a => (forall s. ST s (MVector2 x y s a)) -> Vector2 x y a
create v2m = Vector2{..}
  where
    action :: ST s (Int, MV.MVector s a)
    action = do
      MVector2{..} <- v2m
      pure (num_Ys, mvector)
    (num_Ys, vector) = V.createT action
{-# INLINE create #-}

fromList :: V.Unbox a => [[a]] -> Vector2 x y a
fromList lst = case lst of
  [] -> Vector2 0 V.empty
  (ylst:xs) ->
    let num_Ys = length ylst
    in if all ((== num_Ys) . length) xs
        then Vector2 num_Ys (V.fromList $ concat lst)
        else error "Data.Vector2.Unboxed.fromList given list of unequal length lists"
{-# INLINE fromList #-}

--
size :: V.Unbox a => Vector2 x y a -> (Int, Int) -- (xs, ys)
size Vector2{..} =
  (V.length vector `div` num_Ys, num_Ys)
{-# INLINE size #-}

read :: (V.Unbox a, Enum x, Enum y)
  => Vector2 x y a -> x -> y -> a
read Vector2{..} (fromEnum -> x) (fromEnum -> y) =
  V.unsafeIndex vector (num_Ys * x + y)
{-# INLINE read #-}

mapM_ :: (Monad m, V.Unbox a) => (a -> m ()) -> Vector2 x y a -> m ()
mapM_ f Vector2{..} = V.mapM_ f vector
{-# INLINE mapM_ #-}

mapM :: (Monad m, V.Unbox a, V.Unbox b) => (a -> m b) -> Vector2 x y a -> m (Vector2 x y b)
mapM f Vector2{..} =
  Vector2 num_Ys <$> V.mapM f vector
{-# INLINE mapM #-}

imapM :: (Monad m, V.Unbox a, V.Unbox b, Enum x, Enum y)
  => (x -> y -> a -> m b)
  -> Vector2 x y a -> m (Vector2 x y b)
imapM f Vector2{..} =
  Vector2 num_Ys <$> V.imapM g vector
  where
    g i =
      let (x, y) = i `divMod` num_Ys
      in f (toEnum x) (toEnum y)

imapM_ :: (Monad m, V.Unbox a, Enum x, Enum y)
  => (a -> x -> y -> m ())
  -> Vector2 x y a -> m ()
imapM_ f Vector2{..} = V.imapM_ g vector
  where
    g i a =
      let (x, y) = i `divMod` num_Ys
      in f a (toEnum x) (toEnum y)
{-# INLINE imapM_ #-}

-- | Yield just the given row of the vector without copying it.
sliceX :: (Enum x, V.Unbox a)
  => x -- ^ The row to slice
  -> Vector2 x y a
  -> Vector1 y a
sliceX (fromEnum -> x) Vector2{..} =
  Vector1 $ V.unsafeSlice (num_Ys * x) num_Ys vector
{-# INLINE sliceX #-}

unsafeFreeze :: (PrimMonad m, V.Unbox a)
  => MVector2 x y (PrimState m) a
  -> m (Vector2 x y a)
unsafeFreeze MVector2{..} = Vector2 num_Ys <$> V.unsafeFreeze mvector
{-# INLINE unsafeFreeze #-}

unsafeThaw :: (PrimMonad m, V.Unbox a)
  => Vector2 x y a
  -> m (MVector2 x y (PrimState m) a)
unsafeThaw Vector2{..} = MVector2 num_Ys <$> V.unsafeThaw vector
{-# INLINE unsafeThaw #-}

freeze :: (PrimMonad m, V.Unbox a)
  => MVector2 x y (PrimState m) a
  -> m (Vector2 x y a)
freeze MVector2{..} = Vector2 num_Ys <$> V.freeze mvector
{-# INLINE freeze #-}

thaw :: (PrimMonad m, V.Unbox a)
  => Vector2 x y a
  -> m (MVector2 x y (PrimState m) a)
thaw Vector2{..} = MVector2 num_Ys <$> V.thaw vector
{-# INLINE thaw #-}
