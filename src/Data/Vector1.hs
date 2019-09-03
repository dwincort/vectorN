{-# LANGUAGE DeriveAnyClass #-}
module Data.Vector1
  ( Vector1(..)
  , replicate
  , replicateM
  , generateM
  , create
  , fromList
  , fromListN
  , toList
  , size
  , read
  , readSafe
  , (!)
  , head
  , mapM
  , mapM_
  , imapM
  , imapM_
  , zipWith
  , izipWith
  , izipWithM_
  , unsafeFreeze
  , unsafeThaw
  , freeze
  , thaw
  , unbox
  ) where

import Prelude hiding (head, mapM, mapM_, read, replicate, zipWith)

import Control.DeepSeq         (NFData)
import Control.Monad.Primitive
import Control.Monad.ST        (ST)

import Data.Functor.Apply
import Data.Typeable      (Typeable)

import GHC.Generics

import qualified Data.Vector          as V
import           Data.Vector1.Mutable (MVector1(..))
import qualified Data.Vector1.Unboxed as V1U

newtype Vector1 x a = Vector1 { vector :: V.Vector a }
  deriving ( Generic, Eq, Show, Functor, Foldable, Traversable, Typeable, NFData )

instance Apply (Vector1 x) where
  liftF2 = zipWith

--
replicate :: Int -> a -> Vector1 x a
replicate num_Xs a = Vector1 $ V.replicate num_Xs a
{-# INLINE replicate #-}

replicateM :: Monad m => Int -> m a -> m (Vector1 x a)
replicateM num_Xs x = Vector1 <$> V.replicateM num_Xs x
{-# INLINE replicateM #-}

generateM :: (Monad m, Enum x)
  => Int -> (x -> m a) -> m (Vector1 x a)
generateM num_Xs f =
  Vector1 <$> V.generateM num_Xs (f . toEnum)
{-# INLINE generateM #-}

create :: forall x a.
     (forall s. ST s (MVector1 x s a))
  -> Vector1 x a
create mv1 = Vector1 $ V.create (mvector <$> mv1)
{-# INLINE create #-}

fromList :: [a] -> Vector1 x a
fromList = Vector1 . V.fromList
{-# INLINE fromList #-}

fromListN :: Int -> [a] -> Vector1 x a
fromListN n = Vector1 . V.fromListN n
{-# INLINE fromListN #-}

toList :: Vector1 x a -> [a]
toList Vector1{..} = V.toList vector
{-# INLINE toList #-}

--
size :: Vector1 x a -> Int
size Vector1{..} = V.length vector
{-# INLINE size #-}

read :: Enum x
  => Vector1 x a -> x -> a
read Vector1{..} (fromEnum -> x) =
  V.unsafeIndex vector x
{-# INLINE read #-}

readSafe :: Enum x
  => Vector1 x a -> x -> Maybe a
readSafe Vector1{..} (fromEnum -> x)
  | x < V.length vector =
    Just (V.unsafeIndex vector x)
  | otherwise =
    Nothing
{-# INLINE readSafe #-}

(!) :: Enum x
  => Vector1 x a -> x -> a
(!) Vector1{..} (fromEnum -> x) =
  V.unsafeIndex vector x
{-# INLINE (!) #-}

head :: Vector1 x a -> Maybe a
head Vector1{..} = vector V.!? 0
{-# INLINE head #-}

mapM_ :: Monad m => (a -> m ()) -> Vector1 x a -> m ()
mapM_ f Vector1{..} = V.mapM_ f vector
{-# INLINE mapM_ #-}

mapM :: Monad m => (a -> m b) -> Vector1 x a -> m (Vector1 x b)
mapM f Vector1{..} =
  Vector1 <$> V.mapM f vector
{-# INLINE mapM #-}

imapM :: (Monad m, Enum x)
  => (x -> a -> m b)
  -> Vector1 x a -> m (Vector1 x b)
imapM f Vector1{..} =
  Vector1 <$> V.imapM (f . toEnum) vector

imapM_ :: (Monad m, Enum x)
  => (a -> x -> m ())
  -> Vector1 x a -> m ()
imapM_ f Vector1{..} = V.imapM_ (flip f . toEnum) vector
{-# INLINE imapM_ #-}

unsafeFreeze :: PrimMonad m
  => MVector1 x (PrimState m) a
  -> m (Vector1 x a)
unsafeFreeze MVector1{..} = Vector1 <$> V.unsafeFreeze mvector
{-# INLINE unsafeFreeze #-}

unsafeThaw :: PrimMonad m
  => Vector1 x a
  -> m (MVector1 x (PrimState m) a)
unsafeThaw Vector1{..} = MVector1 <$> V.unsafeThaw vector
{-# INLINE unsafeThaw #-}

freeze :: PrimMonad m
  => MVector1 x (PrimState m) a
  -> m (Vector1 x a)
freeze MVector1{..} = Vector1 <$> V.freeze mvector
{-# INLINE freeze #-}

thaw :: PrimMonad m
  => Vector1 x a
  -> m (MVector1 x (PrimState m) a)
thaw Vector1{..} = MVector1 <$> V.thaw vector
{-# INLINE thaw #-}

unbox :: V1U.Unbox a => Vector1 x a -> V1U.Vector1 x a
unbox Vector1{..} = V1U.Vector1 $ V.convert vector
{-# INLINE unbox #-}

zipWith :: (a -> b -> c) -> Vector1 x a -> Vector1 x b -> Vector1 x c
zipWith f (Vector1 a) (Vector1 b) = Vector1 $ V.zipWith f a b
{-# INLINE zipWith #-}

izipWith :: Enum x => (x -> a -> b -> c) -> Vector1 x a -> Vector1 x b -> Vector1 x c
izipWith f (Vector1 a) (Vector1 b) = Vector1 $ V.izipWith (f . toEnum) a b
{-# INLINE izipWith #-}

izipWithM_ :: (Monad m, Enum x) => (x -> a -> b -> m c) -> Vector1 x a -> Vector1 x b -> m ()
izipWithM_ f (Vector1 a) (Vector1 b) = V.izipWithM_ (f . toEnum) a b
{-# INLINE izipWithM_ #-}
