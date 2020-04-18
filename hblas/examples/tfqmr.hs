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

repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  action
  repeatNTimes (n-1) action

{-# INLINE tfqmr #-}
tfqmr :: (Num n, Fractional n, Ord n, Floating n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> V.Vector n
tfqmr a b vec0= 
  do 
    qmr n (m, r, u2, y2, w, y1, k, d, v, u1, theta, eta, tau, rho, x)
    rhon = dot r
    beta = rhon / rho
    rho = rhon
    y1 = axpy' beta w y2 vec0
    u1 = gemv' a y1
    v1 = axpy' beta u2 v vec0
    v = axpy' beta u1 v1 
  where tol = 1e-10
        n = length b
        x = vec0
        r = b
        u1 = x
        u2 = x
        y1 = x
        y2 = x
        w = r
        y1 = r 
        k =0
        d = x
        v = gemv' a y1
        u1 = v
        theta = 0
        eta = 0
        tau = nrm2 r 
        rho = tau * tau
        m = 0
        {-# INLINE qmr #-}
        qmr 0 (_, _, _, _, _, _, _, _, _, _, _, _, _, _, x') =  x'
        qmr n'  (m', r', u2', y2', w', y1', k', d', v', u1', theta', eta', tau', rho', !x') =
          case (tau' * sqrt m+1 <= tol) of
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
                         !zz = gemv' a yy
                         !ss = dot yy zz
                         !tt = (dot rr yy) / ss
                         !xx = axpy' tt yy x' vec0
                     in qmr (n'-1) ( rr, yy, zz, ss, tt, xx)

main :: IO ()
main = do putStr "\n\nTFQMR: \nMatrix size: "
          let !n=2^2
              !symSq=createSymSq 0 n
              !vecX=createVecX' n
              !vec0=createVec0 n
          putStr $ "" ++ (show ( vecX)) ++ "X"
          putStrLn $ "" ++ (show ( symSq))
          t <- getCurrentTime
          --t' <- seq (repeatNTimes 1 (conjgrad symSq vecX vec0)) getCurrentTime
          t' <- seq (tfqmr symSq vecX vec0) getCurrentTime
          putStrLn $ "" ++ (show  (tfqmr symSq vecX vec0))
          putStrLn $ "Time taken (ms): " ++ (show ((diffUTCTime t' t)*1000000) )
          --putStrLn $ "" ++ (show  (round4dp pi ))


          -- test-suite qmr
          --   ghc-options:         -O2
          --                        -dumpdir dumpDirectory/
          --                        -fllvm
          --                        -keep-llvm-files
          --                        -ddump-llvm
          --                        -ddump-asm
          --                        -ddump-to-file
          --   type:                exitcode-stdio-1.0
          --   default-language:    Haskell2010
          --   main-is:             examples/tfqmr.hs
          --   build-depends:       base,
          --                        vector,
          --                        hblas,
          --                        time >= 1.6
