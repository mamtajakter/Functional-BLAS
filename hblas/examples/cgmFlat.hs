{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector as V
import HBLAS.IO.Vector
import HBLAS.Level1
import HBLAS.Level2
import HBLAS.Class
import Data.Time

--a function to round a floating point number to atmost 4 decimal places
round4dp :: (Num n, RealFrac n,  Fractional n, Ord n, Floating n)=>n -> n
round4dp x= fromIntegral (round $ x * 1e4) / 1e4

{-# INLINE repeatNTimes #-}
repeatNTimes 0 _ = return ()
repeatNTimes !n !action =
 do
  action
  repeatNTimes (n-1) action

{-# INLINE conjugateGradient #-}
conjugateGradient :: (Num n, Fractional n, Ord n, Floating n) => V.Vector n -> V.Vector n -> V.Vector n -> V.Vector n

conjugateGradient !a !b !vec0= cgm n ( r, y, z, s, t, x)
  where !tol = 1e-10
        !n = length b
        !minus1 = negate 1
        !ab = gemvFlat a b
        !r = axpy' minus1 ab b vec0
        !y = scal minus1 r
        !z = gemvFlat a y
        !s = dot y z
        !t = (dot y r) / s
        !x = axpy' t y b vec0
        {-# INLINE cgm #-}
        cgm 0 ( _ , _ , _ , _ , _ , !x') =  x'
        cgm !m  ( !r', !y', !z', !s', !t', !x') =
          case (nrm2 r' < tol) of
            True  -> b
            False ->
              let !minust =  minus1 * t'
                  !rr = axpy' minust z' r' vec0
              in case (nrm2 rr < tol) of
                   True  ->  x'
                   False ->
                     let !bb = (dot rr z') / s'
                         !by = scal bb y'
                         !yy = axpy' minus1 rr by vec0
                         !zz = gemvFlat a yy
                         !ss = dot yy zz
                         !tt = (dot rr yy) / ss
                         !xx = axpy' tt yy x' vec0
                     in cgm (m-1) ( rr, yy, zz, ss, tt, xx)

main :: IO ()
main = do putStr "\n\nConjugate Gradient Method: \nMatrix size: "
          let !n=2^12
              !symSq=createFlatMatrix 0 n
              !vecX=createVecX' n
              !vec0=createVec0 n
          putStr $ "" ++ (show (length vecX)) ++ "X"
          putStrLn $ "" ++ (show (length symSq))
          t <- getCurrentTime
          --t' <- seq (repeatNTimes 1 (conjgrad symSq vecX vec0)) getCurrentTime
          t' <- seq (conjugateGradient symSq vecX vec0) getCurrentTime
          -- putStrLn $ "" ++ (show  (conjugateGradient symSq vecX))
          putStrLn $ "Time taken (ms): " ++ (show ((diffUTCTime t' t)*1000000) )
          --putStrLn $ "" ++ (show  (round4dp pi ))


          -- test-suite cg
          --   ghc-options:         -O2
          --                        -dumpdir dumpDirectory/
          --                        -fllvm
          --                        -keep-llvm-files
          --                        -ddump-llvm
          --                        -ddump-asm
          --                        -ddump-to-file
          --   type:                exitcode-stdio-1.0
          --   default-language:    Haskell2010
          --   main-is:             examples/cgm.hs
          --   build-depends:       base,
          --                        vector,
          --                        hblas,
          --                        time >= 1.6


{-

1. Because of laziness, it does not evaluate the parameters first.
add:: Int->Int->Int
add m n = m+n

now if I call add function with a= add 2 (1+4)
b= add a 5

then GHC will replace b=add (add 2 (1+4)) 5
b= add (add 2 5) 5
b= add (2+5) 5
b= add 7 5
b= 7+5
b=12

GHC does all these unnecessary lines because of laziness, GHC does not want to evaluate anything if it does not need to.

So, to force it to work, use bang pattern on each parameters of a function

2. Because fusion does not happen unless if you doent inline it

we know fusion happens when it gets this pattern∷
map f (map g l) = map (f.g) l

so if it gets f= map g l
 and if it also gets  map f g,GHC cant math to the fusion pattern.

 so, in every function type definition, write {-# INLINE function_name #-}
-}
