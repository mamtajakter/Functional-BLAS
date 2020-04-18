module Main where

-- $ ghc -O --make Fibber

-- $ ./Fibber --output fibber.html

import qualified Data.Vector as Vec
import HBLAS.Level1
import HBLAS.IO.Vector (vecFromFile)
import HBLAS.IO.List   (listFromFile)
import Criterion.Main
--import qualified Data.Tuple as T

vec1_10_1,vec2_10_1,vec1_10_2,vec2_10_2,vec1_10_3,vec2_10_3,vec1_10_4,vec2_10_4 :: FilePath
vec1_10_5,vec2_10_5,vec1_10_6,vec2_10_6,vec1_10_7,vec2_10_7,vec1_10_8,vec2_10_8 :: FilePath

vec1_10_1 = "data/vec1_10_1.csv"
vec2_10_1 = "data/vec2_10_1.csv"
vec1_10_2 = "data/vec1_10_2.csv"
vec2_10_2 = "data/vec2_10_2.csv"
vec1_10_3 = "data/vec1_10_3.csv"
vec2_10_3 = "data/vec2_10_3.csv"
vec1_10_4 = "data/vec1_10_4.csv"
vec2_10_4 = "data/vec2_10_4.csv"

vec1_10_5 = "data/vec1_10_5.csv"
vec2_10_5 = "data/vec2_10_5.csv"
vec1_10_6 = "data/vec1_10_6.csv"
vec2_10_6 = "data/vec2_10_6.csv"
vec1_10_7 = "data/vec1_10_7.csv"
vec2_10_7 = "data/vec2_10_7.csv"
vec1_10_8 = "data/vec1_10_8.csv"
vec2_10_8 = "data/vec2_10_8.csv"

uncurry' :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry' f = \(x, y, z) -> f x y z

main :: IO()
main = do
  xvec1_10_1 <- vecFromFile vec1_10_1 :: IO (Vec.Vector Float)
  yvec2_10_1 <- vecFromFile vec2_10_1 :: IO (Vec.Vector Float)
  xvec1_10_2 <- vecFromFile vec1_10_2 :: IO (Vec.Vector Float)
  yvec2_10_2 <- vecFromFile vec2_10_2 :: IO (Vec.Vector Float)
  xvec1_10_3 <- vecFromFile vec1_10_3 :: IO (Vec.Vector Float)
  yvec2_10_3 <- vecFromFile vec2_10_3 :: IO (Vec.Vector Float)
  xvec1_10_4 <- vecFromFile vec1_10_4 :: IO (Vec.Vector Float)
  yvec2_10_4 <- vecFromFile vec2_10_4 :: IO (Vec.Vector Float)

  xvec1_10_5 <- vecFromFile vec1_10_5 :: IO (Vec.Vector Float)
  yvec2_10_5 <- vecFromFile vec2_10_5 :: IO (Vec.Vector Float)
  xvec1_10_6 <- vecFromFile vec1_10_6 :: IO (Vec.Vector Float)
  yvec2_10_6 <- vecFromFile vec2_10_6 :: IO (Vec.Vector Float)
  xvec1_10_7 <- vecFromFile vec1_10_7 :: IO (Vec.Vector Float)
  yvec2_10_7 <- vecFromFile vec2_10_7 :: IO (Vec.Vector Float)
  xvec1_10_8 <- vecFromFile vec1_10_8 :: IO (Vec.Vector Float)
  yvec2_10_8 <- vecFromFile vec2_10_8 :: IO (Vec.Vector Float)
  let alpha,c :: Float
      alpha = 5.0
      c=0.5
      param :: Vec.Vector Float
      param = Vec.fromList (take 5 . repeat $ 2)
                 
  -- t <- getCurrentTime
  defaultMain [
              bgroup "COPY" [  bench "10^1"   $ whnf (uncurry copy) (xvec1_10_1, yvec2_10_1)
                             -- , bench "10^2"   $ whnf (uncurry copy) (xvec1_10_2, yvec2_10_2)
                             -- , bench "10^3"   $ whnf (uncurry copy) (xvec1_10_3, yvec2_10_3)
                             -- , bench "10^4"   $ whnf (uncurry copy) (xvec1_10_4, yvec2_10_4)
                             -- , bench "10^5"   $ whnf (uncurry copy) (xvec1_10_5, yvec2_10_5)
                             -- , bench "10^6"   $ whnf (uncurry copy) (xvec1_10_6, yvec2_10_6)
                             -- , bench "10^7"   $ whnf (uncurry copy) (xvec1_10_7, yvec2_10_7)
                             -- , bench "10^8"   $ whnf (uncurry copy) (xvec1_10_8, yvec2_10_8)
                            ],
              bgroup "SWAP" [  bench "10^1"   $ whnf swap (xvec1_10_1, yvec2_10_1)
                             -- , bench "10^2"   $ whnf swap (xvec1_10_2, yvec2_10_2)
                             -- , bench "10^3"   $ whnf swap (xvec1_10_3, yvec2_10_3)
                             -- , bench "10^4"   $ whnf swap (xvec1_10_4, yvec2_10_4)
                             -- , bench "10^5"   $ whnf swap (xvec1_10_5, yvec2_10_5)
                             -- , bench "10^6"   $ whnf swap (xvec1_10_6, yvec2_10_6)
                             -- , bench "10^7"   $ whnf swap (xvec1_10_7, yvec2_10_7)
                             -- , bench "10^8"   $ whnf swap (xvec1_10_8, yvec2_10_8)
                            ],
              bgroup "AXPY" [  bench "10^1"   $ whnf (uncurry' axpy) (alpha, xvec1_10_1, yvec2_10_1)
                             -- , bench "10^2"   $ whnf (uncurry' axpy) (alpha, xvec1_10_2, yvec2_10_2)
                             -- , bench "10^3"   $ whnf (uncurry' axpy) (alpha, xvec1_10_3, yvec2_10_3)
                             -- , bench "10^4"   $ whnf (uncurry' axpy) (alpha, xvec1_10_4, yvec2_10_4)
                             -- , bench "10^5"   $ whnf (uncurry' axpy) (alpha, xvec1_10_5, yvec2_10_5)
                             -- , bench "10^6"   $ whnf (uncurry' axpy) (alpha, xvec1_10_6, yvec2_10_6)
                             -- , bench "10^7"   $ whnf (uncurry' axpy) (alpha, xvec1_10_7, yvec2_10_7)
                             -- , bench "10^8"   $ whnf (uncurry' axpy) (alpha, xvec1_10_8, yvec2_10_8)
                            ],
              bgroup "SCAL" [  bench "10^1"   $ whnf (uncurry scal) (alpha, xvec1_10_1)
                             -- , bench "10^2"   $ whnf (uncurry scal) (alpha, xvec1_10_2)
                             -- , bench "10^3"   $ whnf (uncurry scal) (alpha, xvec1_10_3)
                             -- , bench "10^4"   $ whnf (uncurry scal) (alpha, xvec1_10_4)
                             -- , bench "10^5"   $ whnf (uncurry scal) (alpha, xvec1_10_5)
                             -- , bench "10^6"   $ whnf (uncurry scal) (alpha, xvec1_10_6)
                             -- , bench "10^7"   $ whnf (uncurry scal) (alpha, xvec1_10_7)
                             -- , bench "10^8"   $ whnf (uncurry scal) (alpha, xvec1_10_8)
                            ],
              bgroup "DOT" [   bench "10^1"   $ whnf (uncurry dot) (xvec1_10_1, yvec2_10_1)
                             -- , bench "10^2"   $ whnf (uncurry dot) (xvec1_10_2, yvec2_10_2)
                             -- , bench "10^3"   $ whnf (uncurry dot) (xvec1_10_3, yvec2_10_3)
                             -- , bench "10^4"   $ whnf (uncurry dot) (xvec1_10_4, yvec2_10_4)
                             -- , bench "10^5"   $ whnf (uncurry dot) (xvec1_10_5, yvec2_10_5)
                             -- , bench "10^6"   $ whnf (uncurry dot) (xvec1_10_6, yvec2_10_6)
                             -- , bench "10^7"   $ whnf (uncurry dot) (xvec1_10_7, yvec2_10_7)
                             -- , bench "10^8"   $ whnf (uncurry dot) (xvec1_10_8, yvec2_10_8)
                            ],
              bgroup "NRM2" [  bench "10^1"   $ whnf nrm2 xvec1_10_1
                             -- , bench "10^2"   $ whnf nrm2 xvec1_10_2
                             -- , bench "10^3"   $ whnf nrm2 xvec1_10_3
                             -- , bench "10^4"   $ whnf nrm2 xvec1_10_4
                             -- , bench "10^5"   $ whnf nrm2 xvec1_10_5
                             -- , bench "10^6"   $ whnf nrm2 xvec1_10_6
                             -- , bench "10^7"   $ whnf nrm2 xvec1_10_7
                             -- , bench "10^8"   $ whnf nrm2 xvec1_10_8
                            ],
              bgroup "ASUM" [  bench "10^1"   $ whnf asum xvec1_10_1
                             -- , bench "10^2"   $ whnf asum xvec1_10_2
                             -- , bench "10^3"   $ whnf asum xvec1_10_3
                             -- , bench "10^4"   $ whnf asum xvec1_10_4
                             -- , bench "10^5"   $ whnf asum xvec1_10_5
                             -- , bench "10^6"   $ whnf asum xvec1_10_6
                             -- , bench "10^7"   $ whnf asum xvec1_10_7
                             -- , bench "10^8"   $ whnf asum xvec1_10_8
                            ],
              bgroup "I_AMAX" [  bench "10^1"   $ whnf idamax xvec1_10_1
                               -- , bench "10^2"   $ whnf idamax xvec1_10_2
                               -- , bench "10^3"   $ whnf idamax xvec1_10_3
                               -- , bench "10^4"   $ whnf idamax xvec1_10_4
                               -- , bench "10^5"   $ whnf idamax xvec1_10_5
                               -- , bench "10^6"   $ whnf idamax xvec1_10_6
                               -- , bench "10^7"   $ whnf idamax xvec1_10_7
                               -- , bench "10^8"   $ whnf idamax xvec1_10_8
                            ],
              bgroup "ROT" [   bench "10^1"   $ whnf (uncurry' rot) (xvec1_10_1, yvec2_10_1, c)
                             -- , bench "10^2"   $ whnf (uncurry' rot) (xvec1_10_2, yvec2_10_2, c)
                             -- , bench "10^3"   $ whnf (uncurry' rot) (xvec1_10_3, yvec2_10_3, c)
                             -- , bench "10^4"   $ whnf (uncurry' rot) (xvec1_10_4, yvec2_10_4, c)
                             -- , bench "10^5"   $ whnf (uncurry' rot) (xvec1_10_5, yvec2_10_5, c)
                             -- , bench "10^6"   $ whnf (uncurry' rot) (xvec1_10_6, yvec2_10_6, c)
                             -- , bench "10^7"   $ whnf (uncurry' rot) (xvec1_10_7, yvec2_10_7, c)
                             -- , bench "10^8"   $ whnf (uncurry' rot) (xvec1_10_8, yvec2_10_8, c)
                            ],
              bgroup "ROTM" [  bench "10^1"   $ whnf (uncurry' rotm) (xvec1_10_1, yvec2_10_1, param)
                             -- , bench "10^2"   $ whnf (uncurry' rotm) (xvec1_10_2, yvec2_10_2, param)
                             -- , bench "10^3"   $ whnf (uncurry' rotm) (xvec1_10_3, yvec2_10_3, param)
                             -- , bench "10^4"   $ whnf (uncurry' rotm) (xvec1_10_4, yvec2_10_4, param)
                             -- , bench "10^5"   $ whnf (uncurry' rotm) (xvec1_10_5, yvec2_10_5, param)
                             -- , bench "10^6"   $ whnf (uncurry' rotm) (xvec1_10_6, yvec2_10_6, param)
                             -- , bench "10^7"   $ whnf (uncurry' rotm) (xvec1_10_7, yvec2_10_7, param)
                             -- , bench "10^8"   $ whnf (uncurry' rotm) (xvec1_10_8, yvec2_10_8, param)
                            ]              
                ]


  -- performance:
  --   main:                hblas_criterion.hs
  --   source-dirs:         tests
  --   ghc-options:
  --   - O2
  --   - -threaded
  --   - -rtsopts
  --   - -with-rtsopts=-N
  --   dependencies:
  --   - hblas


  -- t' <- getCurrentTime
  -- putStrLn $ "time " ++ (show (diffUTCTime t' t))