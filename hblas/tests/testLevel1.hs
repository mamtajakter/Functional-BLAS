module Main where

import Data.Time
import qualified Data.Vector as Vec
import HBLAS.Level1
import HBLAS.IO.Vector
import Data.Time.Clock.POSIX (getPOSIXTime)

-- vec1,vec2 :: FilePath
-- vec1 = "../data/vecX10.csv"
-- vec2 = "../data/vecY10.csv"

main :: IO ()
main = singleVec
  where singleVec :: IO ()
        singleVec =
          do putStrLn "\nBLAS Level-1 test : "
             let c, alpha, beta :: Float
                 param :: Vec.Vector Float
                 param = Vec.fromList (take 5 . repeat $ 2)
                 c = 0.5
                 alpha=1.0
                 beta=1.0
                 n=100
                 vecX= createVecX 0 0 n
                 vecY=vecX
                 vec0= createVec0 n

            --  putStrLn $ "" ++ (show vecX)

             putStrLn $ "Length of vector 1: "
             putStrLn $ "" ++ show (length vecX)
             putStrLn $ "Length of vector 2: "
             putStrLn $ "" ++ show (length vecY)
             putStrLn $ "Vector size " ++ show n ++ ": "
{-}
             t <- getCurrentTime
             t' <- seq (copy vecX vecY) getCurrentTime
             putStrLn $ "COPY: " ++ (show ((diffUTCTime t' t) *1000000))

             t <- getCurrentTime
             t' <- seq (swap (vecX, vecY)) getCurrentTime
             putStrLn $ "SWAP: " ++ (show ((diffUTCTime t' t) *1000000))
-}
             t <- getCurrentTime
             t' <- seq (axpy alpha vecX vecY) getCurrentTime
             putStrLn $ "AXPY: " ++ (show ((diffUTCTime t' t) *1000000))
 {-
             t <- getCurrentTime
             t' <- seq (scal alpha vecX) getCurrentTime
             putStrLn $ "SCAL: " ++ (show ((diffUTCTime t' t) *1000000))

             t <- getCurrentTime
             t' <- seq (dot vecX vecY) getCurrentTime
             putStrLn $ "DOT: " ++ (show ((diffUTCTime t' t) *1000000))

             t <- getCurrentTime
             t' <- seq (nrm2 vecX) getCurrentTime
             putStrLn $ "NRM2: " ++ (show ((diffUTCTime t' t) *1000000))

             t <- getCurrentTime
             t' <- seq (asum vecX) getCurrentTime
             putStrLn $ "ASUM: " ++ (show ((diffUTCTime t' t) *1000000))

             t <- getCurrentTime
             t' <- seq (idamax vecX) getCurrentTime
             putStrLn $ "I_AMAX: " ++ (show ((diffUTCTime t' t) *1000000))

             t <- getCurrentTime
             t' <- seq (rot vecX vecY c) getCurrentTime
             putStrLn $ "ROT: " ++ (show ((diffUTCTime t' t) *1000000))

             t <- getCurrentTime
             t' <- seq (rotm vecX vecY param) getCurrentTime
             putStrLn $ "ROTM: " ++ (show ((diffUTCTime t' t) *1000000))

             t <- getCurrentTime
             t' <- seq (axpy' alpha vecX vecY vec0) getCurrentTime
             putStrLn $ "AXPY': " ++ (show ((diffUTCTime t' t) *1000000))
-}
             t <- getCurrentTime
             t' <- seq (axpyaxpy alpha beta vecX vecY) getCurrentTime
             putStrLn $ "AXPYAXPY : " ++ (show ((diffUTCTime t' t) *1000000))

             --
             -- test-suite level1
             --   ghc-options:         -O2
             --                      -dumpdir dumpopt/
             --                      -ddump-rule-firings
             --                      -ddump-simpl
             --                      -ddump-asm
             --   type:                exitcode-stdio-1.0
             --   default-language:    Haskell2010
             --   main-is:             tests/testLevel1.hs
             --   build-depends:       base,
             --                        vector,
             --                        containers,
             --                        hblas,
             --                        time >= 1.6
