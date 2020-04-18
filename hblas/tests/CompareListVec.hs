module Main where

import Data.Time
import qualified Data.Vector as Vec
import HBLAS.Level1
import HBLAS.IO.Vector (vecFromFile)
import HBLAS.IO.List   (listFromFile)
import Data.Time.Clock.POSIX (getPOSIXTime)

vec1,vec2 :: FilePath
vec1 = "../data/vec1_10_1.csv"
vec2 = "../data/vec2_10_1.csv"

main :: IO ()
main = double >> single >> doubleVec >> singleVec
  where double :: IO ()
        double =
          do x <- listFromFile vec1
             y <- listFromFile vec2
             let c,alpha,a,m,n,d1,d2,x1,x2 :: Double
                 param :: [Double]
                 param = take 5 (repeat 2)
                 c = 0.5
                 alpha=5.0
                 a=5.0
                 m=0.0
                 n= 2.0
                 d1=1000000000.0
                 d2=1.0
                 x1=negate 2.0
                 x2=4.0
             putStrLn "Double Precision List"
             t <- getCurrentTime
             t' <- seq (copy x y) getCurrentTime
             putStrLn $ "COPY: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (swap (x,y)) getCurrentTime
             putStrLn $ "SWAP: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (axpy a x y) getCurrentTime
             putStrLn $ "AXPY: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (scal alpha x) getCurrentTime
             putStrLn $ "SCAL: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (dot x y) getCurrentTime
             putStrLn $ "DOT: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (nrm2 x) getCurrentTime
             putStrLn $ "NRM2: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (asum x) getCurrentTime
             putStrLn $ "ASUM: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (idamax x) getCurrentTime
             putStrLn $ "I_AMAX: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (rot x y c) getCurrentTime
             putStrLn $ "ROT: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (rotm x y param) getCurrentTime
             putStrLn $ "ROTM: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")


        single :: IO ()
        single =
          do x <- listFromFile vec1
             y <- listFromFile vec2
             let c,alpha,a,m,n,d1,d2,x1,x2 :: Float
                 param :: [Float]
                 param = take 5 (repeat 2)
                 c = 0.5
                 alpha=5.0
                 a=5.0
                 m=0.0
                 n= 2.0
                 d1=1000000000.0
                 d2=1.0
                 x1=negate 2.0
                 x2=4.0
             putStrLn "\nSingle Precision List"
             t <- getCurrentTime
             t' <- seq (copy x y) getCurrentTime
             putStrLn $ "COPY: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (swap (x,y)) getCurrentTime
             putStrLn $ "SWAP: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (axpy a x y) getCurrentTime
             putStrLn $ "AXPY: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (scal alpha x) getCurrentTime
             putStrLn $ "SCAL: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (dot x y) getCurrentTime
             putStrLn $ "DOT: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (nrm2 x) getCurrentTime
             putStrLn $ "NRM2: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (asum x) getCurrentTime
             putStrLn $ "ASUM: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (idamax x) getCurrentTime
             putStrLn $ "I_AMAX: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (rot x y c) getCurrentTime
             putStrLn $ "ROT: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (rotm x y param) getCurrentTime
             putStrLn $ "ROTM: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")


        doubleVec :: IO ()
        doubleVec =
          do x<- vecFromFile vec1
             y<- vecFromFile vec2
             let c,alpha,a,m,n,d1,d2,x1,x2 :: Double
                 param :: Vec.Vector Double
                 param = Vec.fromList (take 5 . repeat $ 2)
                 c = 0.5
                 alpha=5.0
                 a=5.0
                 m=0.0
                 n= 2.0
                 d1=1000000000.0
                 d2=1.0
                 x1=negate 2.0
                 x2=4.0
             putStrLn "\nDouble Precision Vector"
             t <- getCurrentTime
             t' <- seq (copy x y) getCurrentTime
             putStrLn $ "COPY: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (swap (x,y)) getCurrentTime
             putStrLn $ "SWAP: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (axpy a x y) getCurrentTime
             putStrLn $ "AXPY: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (scal alpha x) getCurrentTime
             putStrLn $ "SCAL: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (dot x y) getCurrentTime
             putStrLn $ "DOT: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (nrm2 x) getCurrentTime
             putStrLn $ "NRM2: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (asum x) getCurrentTime
             putStrLn $ "ASUM: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (idamax x) getCurrentTime
             putStrLn $ "I_AMAX: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (rot x y c) getCurrentTime
             putStrLn $ "ROT: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (rotm x y param) getCurrentTime
             putStrLn $ "ROTM: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

        singleVec :: IO ()
        singleVec =
          do x <- vecFromFile vec1
             y <- vecFromFile vec2
             let c,alpha,a,m,n,d1,d2,x1,x2 :: Float
                 param :: Vec.Vector Float
                 param = Vec.fromList (take 5 . repeat $ 2)
                 c = 0.5
                 alpha=5.0
                 a=5.0
                 m=0.0
                 n= 2.0
                 d1=1000000000.0
                 d2=1.0
                 x1=negate 2.0
                 x2=4.0
             putStrLn "\nSingle Precision Vector"
             t <- getCurrentTime
             t' <- seq (copy x y) getCurrentTime
             putStrLn $ "COPY: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (swap (x,y)) getCurrentTime
             putStrLn $ "SWAP: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (axpy a x y) getCurrentTime
             putStrLn $ "AXPY: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (scal alpha x) getCurrentTime
             putStrLn $ "SCAL: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (dot x y) getCurrentTime
             putStrLn $ "DOT: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (nrm2 x) getCurrentTime
             putStrLn $ "NRM2: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (asum x) getCurrentTime
             putStrLn $ "ASUM: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (idamax x) getCurrentTime
             putStrLn $ "I_AMAX: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (rot x y c) getCurrentTime
             putStrLn $ "ROT: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             t <- getCurrentTime
             t' <- seq (rotm x y param) getCurrentTime
             putStrLn $ "ROTM: " ++ (show ((diffUTCTime t' t) *1000000) ++ " microsecond")

             putStrLn $ "" ++ (show (cofactorMatrix ))


  -- listVecCompare:
  --   main:                CompareListVec.hs
  --   source-dirs:         tests
  --   ghc-options:
  --   - O2
  --   dependencies:
  --   - hblas


-- test-suite performance
--   ghc-options:         -O2
--   type:                exitcode-stdio-1.0
--   default-language:    Haskell2010
--   main-is:             tests/hblas_criterion.hs
--   build-depends:       base,
--                        vector,
--                        hblas,
--                        time >= 1.6,
--                        criterion


-- test-suite listVecCompare
--   ghc-options:         -O2
--   type:                exitcode-stdio-1.0
--   default-language:    Haskell2010
--   main-is:             tests/CompareListVec.hs
--   build-depends:       base,
--                        vector,
--                        containers,
--                        hblas,
--                        time >= 1.6
