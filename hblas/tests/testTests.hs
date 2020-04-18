{-# LANGUAGE BangPatterns #-}
module Main where

-- import System.Environment (getArgs)
import Control.Parallel (par)
-- import Control.Parallel.Strategies (Strategy, parMap, parList, rseq)


import Data.Time

import HBLAS.Class
import qualified Data.Vector as Vec
import HBLAS.Level1
import HBLAS.Level2
import HBLAS.IO.Vector
import Prelude hiding (map,zipWith,(!!))
import Data.Time.Clock.POSIX (getPOSIXTime)


tp :: Int -> [(Int,Int,Int)]
tp m = [(i,j,k) | i <- [0..(m `div` 3)], j <- [i..(m-i `div` 2)], let k = m- (i+j)]

main2 = print (length (tp 5000))
-- main :: IO ()
-- main = putStrLn "hi"

justCallAxpy :: (HBLAS m, Monoid (m n), Num n, Show (m n))
         => n -> m n -> m n -> m n
justCallAxpy a1 x y  = axpy a1 x y

axpyaxpyB :: (HBLAS m, Monoid (m n), Num n, Show (m n))
         => n -> n -> m n -> m n -> m n
axpyaxpyB a1 a2 x y  = let t1 = axpy a1 x y in axpy a2 t1 y

axpyaxpyBpar :: (HBLAS m, Monoid (m n), Num n, Show (m n))
         => n -> n -> m n -> m n -> m n
axpyaxpyBpar a1 a2 x y  = par  (axpy a1 x y) (par (axpy a1 x y) (axpy a2 x y))

axpyaxpyM :: (HBLAS m, Monoid (m n), Num n, Show (m n))
         => n -> n -> m n -> m n -> m n
axpyaxpyM a1 a2 x y  = axpy a1 (axpy a1 x y) (axpy a2 x y)

axpyaxpy1 :: (HBLAS m, Monoid (m n), Num n, Show (m n))
         => n -> n -> m n -> m n -> m n
axpyaxpy1 a1 a2 x y  = axpy a1 (axpy a2 x y) y

axpyaxpy2 :: (HBLAS m, Monoid (m n), Num n, Show (m n))
         => n -> n -> m n -> m n -> m n -> m n -> (m n, m n)
axpyaxpy2 a1 a2 x1 y1 x2 y2  = (x3,y3)
                     where
                     x3= axpy a1 x1 y1
                     y3= axpy a2 x2 y2

-- axpyaxpy3 :: (HBLAS m, Monoid (m n), Num n, Show (m n))
--          => m n -> m n
-- axpyaxpy3 x  = map ((scalTest 5) . axpyTest) x

--x=y
vec1,vec2,vec11,vec22,vec111,vec222,mat1 :: FilePath

vec1 = "data/vec1_10_7.csv"
vec2 = "data/vec2_10_7.csv"

vec13 = "data/vec1_10_7.csv"
vec23 = "data/vec2_10_7.csv"

vec11 = "data/vec11_10_7.csv"
vec22 = "data/vec22_10_7.csv"

vec111 = "data/vec111_10_7.csv"
vec222 = "data/vec222_10_7.csv"

vec1x = "data/vec1x_10_7.csv"
vec2y = "data/vec2y_10_7.csv"

mat1 = "data/mat_10.csv"

main :: IO ()
main = singleVec
  where singleVec :: IO ()
        singleVec =
          do x1<- vecFromFile vec1
             y1<- vecFromFile vec2

             x2<- vecFromFile vec1
             y2<- vecFromFile vec2

             x11<- vecFromFile vec11
             y11<- vecFromFile vec22

             x111<- vecFromFile vec111
             y111<- vecFromFile vec222

             x3<- vecFromFile vec13
             y3<- vecFromFile vec23

             xx<- vecFromFile vec1x
             yy<- vecFromFile vec2y

             tmp1 <- vecFromFile vec1
             xyA <- matrixVecFromFile mat1
             let alpha1, alpha2, alpha3 :: Float
                 alpha1 = 5.2
                 alpha2 = 6.3
                 alpha3 = 3.1
             putStrLn "\nSingle Precision Vector"

             ---zipwith zipwith fold/build
             t <- getCurrentTime
             t' <- seq (justCallAxpy alpha1 x1 y1) getCurrentTime
             putStrLn $ "Just Call AXPY: " ++ (show ((diffUTCTime t' t) ) ++ " seconds")
             t <- getCurrentTime
             t' <- seq (justCallAxpy alpha2 x1 y1) getCurrentTime
             putStrLn $ "Just Call AXPY: " ++ (show ((diffUTCTime t' t) ) ++ " seconds")

             ----zipwith zipWith fold/build fold/build
             t <- getCurrentTime
             t' <- seq (axpyaxpyB alpha1 alpha2 x111 y111) getCurrentTime
             putStrLn $ "AXPY in AXPY  (2 axpys): " ++ (show ((diffUTCTime t' t) ) ++ " seconds")

             t <- getCurrentTime
             t' <- seq (axpyaxpyB alpha1 alpha3 x111 y111) getCurrentTime
             putStrLn $ "AXPY in AXPY  (2 axpys): " ++ (show ((diffUTCTime t' t) ) ++ " seconds")

             ----zipwith zipWith fold/build fold/build
             t <- getCurrentTime
             t' <- seq (axpyaxpyBpar alpha1 alpha2 x3 y3) getCurrentTime
             putStrLn $ "PAR AXPY in AXPY  (2 axpys): " ++ (show ((diffUTCTime t' t) ) ++ " seconds")

             t <- getCurrentTime
             t' <- seq (axpyaxpyBpar alpha1 alpha3 x3 y3) getCurrentTime
             putStrLn $ "PAR AXPY in AXPY  (2 axpys): " ++ (show ((diffUTCTime t' t) ) ++ " seconds")

             ----zipwith zipwith fold/build
             ---stream/unstream stream/unstream
             t <- getCurrentTime
             t' <- seq (axpyaxpyM alpha1 alpha2 x2 y2) getCurrentTime
             putStrLn $ "AXPY using two AXPY (3 axpys): " ++ (show ((diffUTCTime t' t) ) ++ " seconds")

             t <- getCurrentTime
             t' <- seq (axpyaxpyM alpha1 alpha3 x2 y2) getCurrentTime
             putStrLn $ "AXPY using two AXPY (3 axpys): " ++ (show ((diffUTCTime t' t) ) ++ " seconds")

             ----zipwith zipwith stream/unstream
             --- zipwith stream unstream
             ---fold/build fold/build
             t <- getCurrentTime
             t' <- seq (axpyaxpy1 alpha1 alpha2 xx yy) getCurrentTime
             putStrLn $ " AXPY using one AXPY (2 axpys): " ++ (show ((diffUTCTime t' t) ) ++ " seconds")

             t <- getCurrentTime
             t' <- seq (axpyaxpy1 alpha1 alpha3 xx yy) getCurrentTime
             putStrLn $ "AXPY using one AXPY (2 axpys): " ++ (show ((diffUTCTime t' t) ) ++ " seconds")

             -- t <- getCurrentTime
             -- (aaa,bbb)<- axpyaxpy2 alpha1 alpha2 x1 y1 x2 y2
             -- t' <-  getCurrentTime
             -- putStrLn $ "Ajaira AXPY using one AXPY: " ++ (show ((diffUTCTime t' t) ) ++ " seconds")
             -- putStrLn (aaa!!0)

main1 :: IO ()
main1 = do
  { !a <- matrixVecFromFile "data/mat_10.csv"
  ; print (length a)
  ; t <- getCurrentTime
  ; t' <-  getCurrentTime
  ; putStrLn $ "Matrix size: 10X10: " ++ (show (diffUTCTime t' t) ) }


-- Just Call AXPY twice: 25.02935s seconds
-- AXPY using another one AXPY : 10.464947s seconds
-- AXPY using two AXPY: 15.781722s seconds
-- Ajaira AXPY using one AXPY: 11.453381s seconds
