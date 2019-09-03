{-# LANGUAGE DeriveAnyClass #-}
module Data.Vector1.Unboxed
  ( Vector1(..)
  , V.Unbox
  , replicate
  , replicateM
  , generateM
  , create
  , fromList
  , fromListN
  , size
  , read
  , (!)
  , mapM
  , mapM_
  , imapM
  , imapM_
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
import           Data.Vector1.Unboxed.Mutable (MVector1(..))

newtype Vector1 x a = Vector1 { vector :: V.Vector a }
  deriving ( Generic, Show, Typeable, NFData, Eq )

--
replicate :: V.Unbox a => Int -> a -> Vector1 x a
replicate num_Xs a =
  Vector1 $ V.replicate num_Xs a
{-# INLINE replicate #-}

replicateM :: (Monad m, V.Unbox a) => Int -> m a -> m (Vector1 x a)
replicateM num_Xs a =
  Vector1 <$> V.replicateM num_Xs a
{-# INLINE replicateM #-}

generateM :: (Monad m, V.Unbox a, Enum x)
  => Int -> (x -> m a) -> m (Vector1 x a)
generateM num_Xs f =
  Vector1 <$> V.generateM num_Xs (f . toEnum)
{-# INLINE generateM #-}

create :: forall x a. V.Unbox a => (forall s. ST s (MVector1 x s a)) -> Vector1 x a
create mv1 = Vector1 $ V.create (mvector <$> mv1)
{-# INLINE create #-}

fromList :: V.Unbox a => [a] -> Vector1 x a
fromList = Vector1 . V.fromList
{-# INLINE fromList #-}

fromListN :: V.Unbox a => Int -> [a] -> Vector1 x a
fromListN n = Vector1 . V.fromListN n
{-# INLINE fromListN #-}

--
size :: V.Unbox a => Vector1 x a -> Int
size Vector1{..} = V.length vector
{-# INLINE size #-}

(!) :: (V.Unbox a, Enum x)
  => Vector1 x a -> x -> a
(!) Vector1{..} (fromEnum -> x) =
  V.unsafeIndex vector x
{-# INLINE (!) #-}

read :: (V.Unbox a, Enum x)
  => Vector1 x a -> x -> a
read Vector1{..} (fromEnum -> x) =
  V.unsafeIndex vector x
{-# INLINE read #-}

mapM_ :: (Monad m, V.Unbox a) => (a -> m ()) -> Vector1 x a -> m ()
mapM_ f Vector1{..} = V.mapM_ f vector
{-# INLINE mapM_ #-}

mapM :: (Monad m, V.Unbox a, V.Unbox b) => (a -> m b) -> Vector1 x a -> m (Vector1 x b)
mapM f Vector1{..} =
  Vector1 <$> V.mapM f vector
{-# INLINE mapM #-}

imapM :: (Monad m, V.Unbox a, V.Unbox b, Enum x)
  => (x -> a -> m b)
  -> Vector1 x a -> m (Vector1 x b)
imapM f Vector1{..} =
  Vector1 <$> V.imapM (f . toEnum) vector

imapM_ :: (Monad m, V.Unbox a, Enum x)
  => (a -> x -> m ())
  -> Vector1 x a -> m ()
imapM_ f Vector1{..} = V.imapM_ (flip f . toEnum) vector
{-# INLINE imapM_ #-}

unsafeFreeze :: (PrimMonad m, V.Unbox a)
  => MVector1 x (PrimState m) a
  -> m (Vector1 x a)
unsafeFreeze MVector1{..} = Vector1 <$> V.unsafeFreeze mvector
{-# INLINE unsafeFreeze #-}

unsafeThaw :: (PrimMonad m, V.Unbox a)
  => Vector1 x a
  -> m (MVector1 x (PrimState m) a)
unsafeThaw Vector1{..} = MVector1 <$> V.unsafeThaw vector
{-# INLINE unsafeThaw #-}

freeze :: (PrimMonad m, V.Unbox a)
  => MVector1 x (PrimState m) a
  -> m (Vector1 x a)
freeze MVector1{..} = Vector1 <$> V.freeze mvector
{-# INLINE freeze #-}

thaw :: (PrimMonad m, V.Unbox a)
  => Vector1 x a
  -> m (MVector1 x (PrimState m) a)
thaw Vector1{..} = MVector1 <$> V.thaw vector
{-# INLINE thaw #-}
