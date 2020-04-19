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


tfqmr :: (Num n, Fractional n, Ord n, Floating n) => V.Vector (V.Vector n) -> V.Vector n -> n-> n->V.Vector n -> V.Vector n
tfqmr a b tol n vec0= qmr k (m, r, w, y1, d, v, u1, theta, eta, tau, rho, x)
  where x = vec0 
        r = b
        w = r
        y1 = r 
        k = 1
        d = vec0
        v = gemv' a y1
        u1 = v
        theta = 0
        eta = 0
        tau = nrm2 r 
        rho = tau * tau
        m = 0
        qmr 100 (_, _, _, _, _, _, _,  _, _, _, _, x') =  x'
        qmr k'  (m', r',  w', y1', d', v', u1', theta', eta', tau', rho', x') =
          let sigma =  dot r' v'  
              alpha = rho' / sigma 
              j = 1
              mm' = 2 * k' - 2 + j 
              ww' = axpy ((negate 1) * alpha)  u1' w' 
              dd' = axpy (theta' * theta' * eta'/ alpha) d' y1'    
              ttheta' = (nrm2 ww') / tau' 
              c = 1 / sqrt (1 + ttheta' * ttheta') 
              ttau' = tau' * ttheta' * c 
              eeta' = c * c * alpha 
              xx' = axpy eeta'  dd' x'
          in case ((ttau' * sqrt (mm' + 1)) <  tol) of
              True  -> xx'
              False ->
                let jj = 2
                    yy2 = axpy ((negate 1) * alpha)  v' y1'
                    uu2 = gemv' a yy2
                    mm = 2 * k' - 2 + jj
                    ww = axpy ((negate 1) * alpha) uu2 ww' 
                    dd = axpy (ttheta' * ttheta' * eeta'/ alpha) dd' yy2  
                    ttheta = nrm2 ww / ttau'
                    cc = 1 / sqrt (1 + ttheta * ttheta) 
                    ttau = ttau' * ttheta * cc
                    eeta = cc * cc * alpha 
                    xx = axpy eeta  dd xx'  
                in case ((ttau * sqrt (mm + 1)) <  tol) of
                     True  ->  xx
                     False ->
                       let rhon = dot r' ww
                           beta = rhon / rho'
                           rrho = rhon
                           yy1 = axpy beta yy2 ww   
                           uu1 = gemv' a yy1
                           vv = axpyaxpy' beta beta  v' uu2 uu1 
                       in qmr (k'+1) (mm, r', ww, yy1, dd, vv, uu1, ttheta, eeta, ttau, rrho, xx)

       
main :: IO ()
main = do putStr "\n\nTFQMR: \nMatrix size: "
          let !n=2^2
              !symSq=createUnSymSq 0 n
              !vecX=createVecX' n
              !vec0=createVec0 n
          putStr $ "" ++ (show ( vecX)) ++ "X"
          putStrLn $ "" ++ (show ( symSq))
          t <- getCurrentTime
          --t' <- seq (repeatNTimes 1 (conjgrad symSq vecX vec0)) getCurrentTime
          t' <- seq (tfqmr symSq vecX 1e-10 4 vec0) getCurrentTime
          putStrLn $ "" ++ (show  (tfqmr symSq vecX 1e-10 4 vec0))
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
