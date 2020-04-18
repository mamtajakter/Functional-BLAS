{-# LANGUAGE ConstraintKinds #-}
module HBLAS.Class where

import Data.Monoid
import qualified Data.List as List
import qualified Data.Vector as Vec

type HBLAS m = (Applicative m, Mappable m, Zippable m, Indexable m, Foldable m)

class Foldable m => Zippable m where
  zipWith :: (a -> b -> c) -> m a -> m b -> m c

instance Zippable [] where
  zipWith = List.zipWith

instance Zippable Vec.Vector where
  zipWith = Vec.zipWith

class Foldable m => Mappable m where
  map :: (a -> b ) -> m a -> m b
  
instance Mappable [] where
  map = List.map

instance Mappable Vec.Vector where
  map = Vec.map

class Indexable m where
  (!!) :: m a -> Int -> a
  elemIndex :: (Eq a) => a -> m a -> Maybe Int

instance Indexable [] where
  (!!) = (List.!!)
  elemIndex = List.elemIndex

instance Indexable Vec.Vector where
  (!!) = (Vec.!)
  elemIndex = Vec.elemIndex

replicateHBlas :: (Applicative m, HBLAS m, Monoid (m n)) => Int -> n -> m n
replicateHBlas 0 _ = mempty
replicateHBlas n x = pure x <> replicateHBlas (n-1) x
