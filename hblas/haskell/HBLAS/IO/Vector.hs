{-# LANGUAGE BangPatterns #-}
module HBLAS.IO.Vector where

import Data.Vector as V
import Data.Monoid

-- vecFromFile :: (Floating n, Read n) => FilePath -> IO (Vector n)
-- vecFromFile f = fmap read . fromList . lines <$> readFile f
--
-- matrixVecFromFile :: (Floating n, Read n) => FilePath -> IO (Vector (Vector n))
-- matrixVecFromFile f =   fromList
--                  .   fmap (fromList . read . (<>"]") . ("["<>)) . lines
--                  <$> readFile f
{-
createb :: (Num n) =>  Vector n
createb = pure (fromIntegral ( 1) ) <> pure (fromIntegral (2) ) <> empty

createx :: (Num n) =>  Vector n
createx = pure (fromIntegral (0) ) <> pure (fromIntegral (0) ) <> empty

createA1 :: (Num n) =>  Vector n
createA1 = pure (fromIntegral (1) ) <> pure (fromIntegral (2) ) <> empty

createA2 :: (Num n) =>  Vector n
createA2 = pure (fromIntegral (2) ) <> pure (fromIntegral (3) ) <> empty

createA :: (Num n) =>  Vector (Vector n)
createA = pure (createA1 ) <> pure (createA2 ) <> empty
-}
-- ---- commented on Octobre 16th
{-# INLINE createVecX #-}
createVecX :: (Num n) => Int->Int ->Int-> Vector n
createVecX !i !j !n
             | i<n  = pure (fromIntegral (j `mod` 9) +1) <> (createVecX (i+1) (j+1) n)
             | otherwise= empty



{-# INLINE createVecX' #-}
createVecX' :: (Num n) => Int-> Vector n
createVecX' !n =V.replicate n 1

{-# INLINE createVec0 #-}
createVec0 :: (Num n) => Int-> Vector n
createVec0 !n =V.replicate n 0

{-# INLINE createUnSymSq #-}
createUnSymSq :: (Num n) => Int->Int  -> (Vector (Vector n ))
createUnSymSq !i !n
             | i<n  =  pure (createVecX 0 (i `mod` 2) n) <> (createUnSymSq (i+1) n)
             | otherwise = empty

---- commented on Octobre 16th
{-# INLINE createSymSq #-}
createSymSq :: (Num n) => Int->Int  -> (Vector (Vector n ))
createSymSq !i !n
             | i<n  =  pure (createVecX 0 (i `mod` 9) n) <> (createSymSq (i+1) n)
             | otherwise = empty

{-# INLINE createFlatMatrix #-}
createFlatMatrix :: (Num n) => Int->Int  -> Vector n
createFlatMatrix !i !n
             | i<n  =  createVecX 0 (i `mod` 9) n <> (createFlatMatrix (i+1) n)
             | otherwise = empty
--
---- commented on Octobre 16th
{-# INLINE helperSymLowSq #-}
helperSymLowSq :: (Num n) => Int->Int ->Int->Int-> Vector n
helperSymLowSq !i !j !k !n
             | i<=k  = pure (fromIntegral (j `mod` 9) +1) <> (helperSymLowSq (i+1) (j+1) k n)
             | i<n  = pure (0) <> (helperSymLowSq (i+1) (j+1) k n)
             | otherwise= empty

-- commented on Octobre 16th
{-# INLINE createSymLowSq #-}
createSymLowSq :: (Num n) => Int->Int  -> (Vector (Vector n ))
createSymLowSq !i !n
             | i<n  =  pure (helperSymLowSq 0 (i `mod` 9) i n) <> (createSymLowSq (i+1) n)
             | otherwise = empty

-- commented on Octobre 16th
{-# INLINE helperSymUpSq #-}
helperSymUpSq :: (Num n) => Int->Int ->Int->Int-> Vector n
helperSymUpSq !i !j !k !n
             | i<k  = pure (0) <> (helperSymUpSq (i+1) (j+1) k n)
             | i<n  = pure (fromIntegral (j `mod` 9) +1) <> (helperSymUpSq (i+1) (j+1) k n)
             | otherwise= empty

-- commented on Octobre 16th
{-# INLINE createSymUpSq #-}
createSymUpSq :: (Num n) => Int->Int  -> (Vector (Vector n ))
createSymUpSq !i !n
             | i<n  =  pure (helperSymUpSq 0 (i `mod` 9) i n) <> (createSymUpSq (i+1) n)
             | otherwise = empty

-- commented on Octobre 16th
{-# INLINE helperSymLowTri #-}
helperSymLowTri :: (Num n) => Int->Int ->Int->Int-> Vector n
helperSymLowTri !i !j !k !n
             | i<=k  = pure (fromIntegral (j `mod` 9) +1) <> (helperSymLowTri (i+1) (j+1) k n)
             | otherwise= empty

-- commented on Octobre 16th
{-# INLINE createSymLowTri #-}
createSymLowTri :: (Num n) => Int->Int  -> (Vector (Vector n ))
createSymLowTri !i !n
             | i<n  =  pure (helperSymLowTri 0 (i `mod` 9) i n) <> (createSymLowTri (i+1) n)
             | otherwise = empty

-- commented on Octobre 16th
{-# INLINE helperSymUpTri #-}
helperSymUpTri :: (Num n) => Int->Int ->Int->Int-> Vector n
helperSymUpTri !i !j !k !n
             | i<k     = pure (0 ) <> (helperSymUpTri (i+1) (j+1) k n)
             | i<n  = pure (fromIntegral (j `mod` 9)+1 ) <> (helperSymUpTri (i+1) (j+1) k n)
             | otherwise= empty

-- commented on Octobre 16th
{-# INLINE createSymUpTri #-}
createSymUpTri :: (Num n) => Int->Int  -> (Vector (Vector n ))
createSymUpTri !i !n
             |i<n  =  pure (V.drop i (helperSymUpTri 0 (i `mod` 9) i n)) <> (createSymUpTri (i+1) n)
             | otherwise = empty

{-

update :: ( Num n)=> [n]-> Int-> n-> [n]
update (x:xs) index value
       | index>0       = x : (update xs (index-1) value)
       | otherwise = value : xs

-}
