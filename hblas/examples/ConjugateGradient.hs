{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector as V
import HBLAS.IO.Vector
import HBLAS.Level1
import HBLAS.Level2
import HBLAS.Class
import Data.Time

repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  action
  repeatNTimes (n-1) action

-- conjgrad :: (HBLAS m, Monoid (m n), Num n, Show (m n))
--          => m (m n) -> m n -> m n
conjgrad :: (Num n, Fractional n, Ord n, Floating n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> V.Vector n
conjgrad a b vec0= calc n (b,b,start) id
  where tol = 1e-10
        n = length b
        start = replicateHBlas n 0
        calc 0 (_   ,_   ,x)    k = k x
        calc m (r,p,x) k =
          let ap = gemv' a p
              alpha = (dot r r) / (dot p ap)
              x' = axpy alpha    p  x
              r' = axpy (-alpha) ap r
          in case nrm2 r' < tol of
               True  -> k x'
               False ->
                 let beta = (dot r' r') / (dot r r)
                     p' = axpy beta r' p
                 in calc (m-1) (r',p',x') k

main :: IO ()
main = do putStrLn "\nConjugate Gradient Method: "
          let n=10
              symSq=createA
              vecX=createb
              vec0=createx
          putStrLn $ "CG "
          putStrLn $ "" ++ (show  vecX)
          putStrLn $ "" ++ (show  symSq)
          t <- getCurrentTime
          --t' <- seq (repeatNTimes 1 (conjgrad symSq vecX vec0)) getCurrentTime
          t' <- seq (conjgrad symSq vecX vec0) getCurrentTime
          putStrLn $ "Matrix size " ++ show n ++ "X" ++ show n ++ ": "++ (show ((diffUTCTime t' t)*1000000) )
          putStrLn $ "" ++ (show  (conjgrad symSq vecX vec0))



-- test-suite cg
--   ghc-options:         -O2
--   type:                exitcode-stdio-1.0
--   default-language:    Haskell2010
--   main-is:             examples/ConjugateGradient.hs
--   build-depends:       base,
--                        vector,
--                        hblas,
--                        time >= 1.6
--
