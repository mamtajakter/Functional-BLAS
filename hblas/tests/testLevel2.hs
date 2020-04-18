{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Parallel (par)
import Data.Time
import HBLAS.Class
import qualified Data.Vector as Vec
import HBLAS.Level1
import HBLAS.Level2
import HBLAS.IO.Vector
import Prelude hiding (map,zipWith,(!!))
import Data.Time.Clock.POSIX (getPOSIXTime)

main :: IO ()
main = singleVec
  where singleVec :: IO ()
        singleVec =
          do putStrLn "\nBLAS Level-2 Test: "
             let alpha, beta :: Float
                 alpha = 1.0
                 beta = 1.0
                 n=10000
                 vecX= createVecX 0 0 n
                 vecY=createVecX 0 0 n
                 symSq=createSymSq 0 n
                 symLowSq=createSymLowSq 0 n
                 symUpSq=createSymUpSq 0 n
                 symLowTri=createSymLowTri 0 n
                 symUpTri=createSymUpTri 0 n

            --  putStrLn $ "\nvecX: " ++ (show vecX)
            --  putStrLn $ "\nvecY: " ++ (show vecY)
            --  putStrLn $ "\nsymSq: " ++ (show symSq)
            --  putStrLn $ "\nsymLowSq: " ++ (show symLowSq)
            --  putStrLn $ "\nsymUpSq: " ++ (show  symUpSq)
            --  putStrLn $ "\nsymLowTri: " ++ (show symLowTri)
            --  putStrLn $ "\nsymUpTri: " ++ (show symUpTri)

             putStrLn $ "" ++ (show (length vecX))
             putStrLn $ "" ++ (show (length vecY))
             putStrLn $ "" ++ (show (length symSq))
             putStrLn $ "" ++ (show (length symLowSq))
             putStrLn $ "" ++ (show (length symUpSq))
             putStrLn $ "" ++ (show (length symLowTri))
             putStrLn $ "" ++ (show (length symUpTri))
             putStrLn $ "Matrix size " ++ show n ++ "X" ++ show n ++ ": "
{-
             t <- getCurrentTime
             t' <- seq (gemv symSq vecX vecY alpha beta) getCurrentTime
             putStrLn $ "GEMV : " ++ (show ((diffUTCTime t' t)*1000000 ))

             t <- getCurrentTime
             t' <- seq (ger symSq vecX vecY alpha) getCurrentTime
             putStrLn $ "GER: " ++ (show ((diffUTCTime t' t)*1000000 ))
-}
{-
             t <- getCurrentTime
             t' <- seq (trmv symLowSq vecX 'l') getCurrentTime
             putStrLn $ "TRMV LOWER: " ++ (show ((diffUTCTime t' t)*1000000 ))

             t <- getCurrentTime
             t' <- seq (trmv symUpSq vecX 'u') getCurrentTime
             putStrLn $ "TRMV UPPER: " ++ (show ((diffUTCTime t' t)*1000000 ))

             t <- getCurrentTime
             t' <- seq (trsv symLowSq vecX 'l') getCurrentTime
             putStrLn $ "TRSV LOWER: " ++ (show ((diffUTCTime t' t)*1000000 ))

             t <- getCurrentTime
             t' <- seq (trsv symUpSq vecX 'u') getCurrentTime
             putStrLn $ "TRSV UPPER: " ++ (show ((diffUTCTime t' t)*1000000 ))
-}
             t <- getCurrentTime
             t' <- seq (symv symLowTri vecX vecY alpha beta 'l') getCurrentTime
             putStrLn $ "SYMV LOWER: " ++ (show ((diffUTCTime t' t)*1000000 ))

             t <- getCurrentTime
             t' <- seq (symv symUpTri vecX vecY alpha beta 'u') getCurrentTime
             putStrLn $ "SYMV UPPER: " ++ (show ((diffUTCTime t' t)*1000000 ))

             t <- getCurrentTime
             t' <- seq (syr symLowTri vecX alpha 'l') getCurrentTime
             putStrLn $ "SYR LOWER: " ++ (show ((diffUTCTime t' t)*1000000 ))

             t <- getCurrentTime
             t' <- seq (syr symUpTri vecX alpha 'u') getCurrentTime
             putStrLn $ "SYR UPPER: " ++ (show ((diffUTCTime t' t)*1000000 ))

             t <- getCurrentTime
             t' <- seq (syr2 symLowTri vecX vecY alpha 'l') getCurrentTime
             putStrLn $ "SYR2 LOWER: " ++ (show ((diffUTCTime t' t)*1000000 ))

             t <- getCurrentTime
             t' <- seq (syr2 symUpTri vecX vecY alpha 'u') getCurrentTime
             putStrLn $ "SYR2 UPPER: " ++ (show ((diffUTCTime t' t)*1000000 ))

             --
-- test-suite test_level2
--  ghc-options:         -O2
--                       -dumpdir dumpopt/
--                       -ddump-to-file
--                       -ddump-rule-firings
--  type:                exitcode-stdio-1.0
--  default-language:    Haskell2010
--  main-is:             tests/testLevel2.hs
--  build-depends:       base,
--                       vector,
--                       hblas,
--                       parallel,
--                       time >= 1.6
