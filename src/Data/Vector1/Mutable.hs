module Data.Vector1.Mutable
  ( MVector1(..)
  , IOVector1
  , new
  , replicate
  , replicateM
  , size
  , read
  , write
  , modify
  , imapM_
  ) where

import Prelude hiding (read, replicate)

import Control.DeepSeq         (NFData(..))
import Control.Monad.Primitive

import Data.Typeable (Typeable)

import GHC.Generics

import qualified Data.Vector.Mutable as V

type IOVector1 x = MVector1 x RealWorld

newtype MVector1 x s a = MVector1 { mvector :: V.MVector s a }
  deriving ( Generic, Typeable )

instance NFData (MVector1 x s a) where
  rnf (MVector1 _) = ()

--
new :: PrimMonad m => Int -> m (MVector1 x (PrimState m) a)
new num_Xs =
  MVector1 <$> V.unsafeNew num_Xs
{-# INLINE new #-}

replicate :: PrimMonad m => Int -> a -> m (MVector1 x (PrimState m) a)
replicate num_Xs a =
  MVector1 <$> V.replicate num_Xs a
{-# INLINE replicate #-}

replicateM :: PrimMonad m => Int -> m a -> m (MVector1 x (PrimState m) a)
replicateM num_Xs a =
  MVector1 <$> V.replicateM num_Xs a
{-# INLINE replicateM #-}

--
size :: MVector1 x s a -> Int
size MVector1{..} = V.length mvector
{-# INLINE size #-}

read :: (PrimMonad m, Enum x)
  => MVector1 x (PrimState m) a
  -> x -> m a
read MVector1{..} (fromEnum -> x) =
  V.unsafeRead mvector x
{-# INLINE read #-}

write :: (PrimMonad m, Enum x)
  => MVector1 x (PrimState m) a
  -> x -> a -> m ()
write MVector1{..} (fromEnum -> x) a =
  V.unsafeWrite mvector x a
{-# INLINE write #-}

modify :: (PrimMonad m, Enum x)
  => MVector1 x (PrimState m) a
  -> x -> (a -> a) -> m ()
modify MVector1{..} (fromEnum -> x) f =
  V.unsafeModify mvector f x
{-# INLINE modify #-}

imapM_ :: (PrimMonad m, Enum x)
  => (a -> x -> m ())
  -> MVector1 x (PrimState m) a
  -> m ()
imapM_ f MVector1{..} = go 0
  where
    len = V.length mvector
    go i | i >= len = pure ()
    go i = do
      a <- V.unsafeRead mvector i
      f a (toEnum i)
      go (i+1)
{-# INLINE imapM_ #-}
