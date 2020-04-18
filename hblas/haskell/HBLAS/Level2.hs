{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}

module HBLAS.Level2 where

import qualified Data.Vector as V
import qualified Data.Matrix as M
import HBLAS.Level1
import Prelude hiding (zipWith,(!!), foldl')
import Data.Monoid
---- commented on Octobre 16th
{-# INLINE gemv' #-}
gemv' :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n
gemv' !matA !vecX = fmap (dot vecX) matA
------ commented on Octobre 16th
{-# INLINE gemv #-}
gemv :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> n -> V.Vector n
gemv !matA !vecX !vecY !alpha !beta=
    let !x1 = fmap (dot vecX) matA
        !y1= scal beta vecY
    in  axpy alpha x1 y1

{-# INLINE gemvFlat #-}
gemvFlat :: (Num n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n
gemvFlat !matA !vecX = gemvFlatHelper matA (V.replicate n 0) vecX (n-n) n
               where !n= V.length vecX

-- ---- commented on Octobre 16th
{-# INLINE gemvFlatHelper #-}
gemvFlatHelper :: ( Num n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> Int->Int-> V.Vector n
gemvFlatHelper !a !x !b !i !n
              | i>=n      =  x
              | otherwise = let !ai= V.take n a
                                !xi = gemvFlatHelper2 ai b
                                !new_x  = V.update x (pure(i, xi))-- new x= [1,2,0]
                                !newa = V.drop (n+1) a
                            in gemvFlatHelper newa new_x b (i+n) n



{-# INLINE gemvFlatHelper2 #-}
gemvFlatHelper2 :: ( Num n) =>  V.Vector n ->   V.Vector n ->  n
gemvFlatHelper2 !veca !vecx =  sum ( V.zipWith (\x y -> x*y) veca vecx)

-- commented on Octobre 16th
trmv :: ( Num n, Fractional n) => V.Vector (V.Vector n)  -> V.Vector n ->Char -> V.Vector n
trmv a b uplo
          | uplo== 'l'= trmvLower a (V.replicate n 0) b (n-n) n
          | otherwise = trmvUpper a (V.replicate n 0) b (n-n) n
        where n= V.length b

-- commented on Octobre 16th
trmvLower :: ( Num n, Fractional n) => V.Vector (V.Vector n)  -> V.Vector n -> V.Vector n -> Int->Int-> V.Vector n
trmvLower a x b i n
              | i>=n      =  x
              | otherwise = let ai= V.take (i+1) (a  V.! i)
                                bi= V.take (i+1) b
                                xi = trmvHelper ai bi
                                new_x  = V.update x (pure(i, xi))-- new x= [1,2,0]
                            in trmvLower a new_x b (i+1) n

-- commented on Octobre 16th
trmvUpper :: ( Num n, Fractional n) => V.Vector (V.Vector n)  -> V.Vector n -> V.Vector n -> Int->Int-> V.Vector n
trmvUpper a x b i n
              | i>=n      =  x
              | otherwise = let ai= V.drop i (a  V.! i)
                                bi= V.drop i b
                                xi = trmvHelper ai bi
                                new_x  = V.update x (pure (i, xi))-- new x= [1,2,0]
                            in trmvUpper a new_x b (i+1) n

-- commented on Octobre 16th
trmvHelper :: ( Num n) =>  V.Vector n ->   V.Vector n ->  n
trmvHelper veca vecx =  sum ( V.zipWith (\x y -> x*y) veca vecx)

syr2 :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> Char-> V.Vector (V.Vector n)
syr2 matA vecX vecY alpha uplo
                          | uplo=='l'= V.init (syr2Lower1 matA vecX vecY alpha)
                          | otherwise= V.init (syr2Upper1 matA vecX vecY alpha)

-- commented on Octobre 16th
syr2Upper1 :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> V.Vector (V.Vector n)
syr2Upper1 matA vecX vecY alpha=
    let mat= syr2Upper2 matA vecX vecY alpha
        x1 = scal alpha vecY
        y1= getXYTranspose x1 vecX
    in V.zipWith (V.zipWith (+)) mat y1

-- commented on Octobre 16th
syr2Upper2 :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> V.Vector (V.Vector n)
syr2Upper2 matA vecX vecY alpha=
    let n= V.length vecX
        mat= getUpperTotalMatrix matA 0 n
        x1 = scal alpha vecX
        y1= getXYTranspose x1 vecY
    in V.zipWith (V.zipWith (+)) mat y1

-- commented on Octobre 16th
syr2Lower1 :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> V.Vector (V.Vector n)
syr2Lower1 matA vecX vecY alpha=
    let mat= syr2Lower2 matA vecX vecY alpha
        x1 = scal alpha vecY
        y1= getXYTranspose x1 vecX
    in V.zipWith (V.zipWith (+)) mat y1

-- commented on Octobre 16th
syr2Lower2 :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> V.Vector (V.Vector n)
syr2Lower2 matA vecX vecY alpha=
    let n= V.length vecX
        mat= getLowerTotalMatrix matA 0 n
        x1 = scal alpha vecX
        y1=  getXYTranspose x1 vecY
    in V.zipWith (V.zipWith (+)) mat y1

syr :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> n -> Char-> V.Vector (V.Vector n)
syr matA vecX alpha uplo
                          | uplo=='l'= V.init (syrLower matA vecX alpha)
                          | otherwise= V.init (syrUpper matA vecX alpha)


-- commented on Octobre 16th
syrUpper :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> n -> V.Vector (V.Vector n)
syrUpper matA vecX alpha=
    let n= V.length vecX
        mat= getUpperTotalMatrix matA 0 n
        x1 = scal alpha vecX
        y1= getXYTranspose x1 vecX
    in V.zipWith (V.zipWith (+)) mat y1

syrLower :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> n -> V.Vector (V.Vector n)
syrLower matA vecX alpha=
    let n= V.length vecX
        mat= getLowerTotalMatrix matA 0 n
        x1 = scal alpha vecX
        y1= getXYTranspose x1 vecX
    in V.zipWith (V.zipWith (+)) mat y1

-- commented on Octobre 16th
ger :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> V.Vector (V.Vector n)
ger matA vecX vecY alpha=
    let x1 = scal alpha vecX
        y1= getXYTranspose x1 vecY
    in V.zipWith (V.zipWith (+)) matA y1

--getXYTranspose gets vector x and vector y and returns the matrix= x * (y transpose)

-- commented on Octobre 16th
getXYTranspose :: ( Num n) => V.Vector n ->V.Vector n -> (V.Vector (V.Vector n))
getXYTranspose x y
              | null x =  V.empty <> V.empty
              | otherwise =  pure (scal (V.head x) y) <> getXYTranspose (V.tail x) y

-- commented on Octobre 16th
trsv :: ( Num n, Fractional n) => V.Vector (V.Vector n)  -> V.Vector n ->Char -> V.Vector n
trsv a b uplo
          | uplo== 'l'= fst(trsvLower a (V.replicate n 0) b (n-n) n)
          | otherwise = fst(trsvUpper a (V.replicate n 0) b (n-1) (n-n-1))
        where n= V.length b

-- commented on Octobre 16th
trsvLower :: ( Num n, Fractional n) => V.Vector (V.Vector n)  -> V.Vector n -> V.Vector n -> Int->Int-> (V.Vector n, V.Vector n)
trsvLower a x b i n
              | i>=n      =  (x,b)
              | otherwise = let new_bi = trsvHelper (a  V.! i) x (b  V.! i)-- newbi=6
                                new_b= V.update b (pure (i, new_bi))
                                aii = ((a  V.! i)  V.! i)
                                new_x  = V.update x (pure (i, (new_bi/aii)))-- new x= [1,2,0]
                            in trsvLower a new_x new_b (i+1) n

-- commented on Octobre 16th
trsvUpper :: ( Num n, Fractional n) => V.Vector (V.Vector n)  -> V.Vector n -> V.Vector n -> Int->Int-> (V.Vector n, V.Vector n)
trsvUpper a x b i n
              |  i<=n = (x,b)
              |  otherwise = let new_bi = trsvHelper (a  V.! i) x (b  V.! i)-- newbi=6
                                 new_b= V.update b (pure (i, new_bi))
                                 aii = ((a  V.! i)  V.! i)
                                 new_x  = V.update x (pure (i, (new_bi/aii)))
                             in trsvUpper a new_x new_b (i-1) n

-- commented on Octobre 16th
trsvHelper :: ( Num n) =>  V.Vector n ->   V.Vector n->  n ->  n
trsvHelper veca vecx valueb= valueb -  sum ( V.zipWith (\x y -> x*y) veca vecx)

-- commented on Octobre 16th
symv :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> n -> Char-> V.Vector n
symv matA vecX vecY alpha beta uplo
                          | uplo=='l'= symvLower matA vecX vecY alpha beta
                          | otherwise= symvUpper matA vecX vecY alpha beta

-- commented on Octobre 16th
symvLower :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> n -> V.Vector n
symvLower matA vecX vecY alpha beta =
    let n= V.length vecX
        mat= getLowerTotalMatrix matA 0 n
        x1 = fmap (dot vecX) mat
        y1= scal beta vecY
    in  axpy alpha x1 y1

-- commented on Octobre 16th
symvUpper :: (Num n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> n -> V.Vector n
symvUpper matA vecX vecY alpha beta =
    let n= V.length vecX
        mat= getUpperTotalMatrix matA 0 n
        x1 = fmap (dot vecX) mat
        y1= scal beta vecY
    in  axpy alpha x1 y1

-- commented on Octobre 16th
getLowerTotalMatrix :: ( Num n) =>V.Vector(V.Vector n)-> Int->Int->V.Vector(V.Vector n)
getLowerTotalMatrix a i n
            | i>= n= V.empty <> V.empty
            | otherwise = pure (getLowerTotalRow a x i j n)  <> getLowerTotalMatrix a (i+1) n
        where x= a V.! i
              j=i


-- commented on Octobre 16th
-- x = a V.! i
getLowerTotalRow:: ( Num n) =>  V.Vector(V.Vector n)-> V.Vector n -> Int ->Int->  Int-> V.Vector n
getLowerTotalRow a x i j n
           | i>= n = x
           | otherwise= let aij= ((a  V.! i)  V.! j)
                            x1= V.take i x
                            new_x=  V.snoc x1 aij
                         in getLowerTotalRow a new_x (i+1) j n

-- commented on Octobre 16th
getUpperTotalMatrix :: ( Num n) =>V.Vector(V.Vector n)-> Int->Int->V.Vector(V.Vector n)
getUpperTotalMatrix a i n
            | i>= n= V.empty <> V.empty
            | otherwise = pure (getUpperTotalRow a x i 0 (n-n))  <> getUpperTotalMatrix a (i+1) n
        where x= a V.! i

--n=0
--j=0

-- commented on Octobre 16th
getUpperTotalRow:: ( Num n) =>  V.Vector(V.Vector n)-> V.Vector n -> Int ->Int->Int-> V.Vector n
getUpperTotalRow a x i j n
           | i< n = x
           | j==0  = getUpperTotalRow a x (i-1) (j+1) n
           | otherwise= let aij= ((a  V.! i)  V.! j)
                            new_x=  V.cons aij x
                         in getUpperTotalRow a new_x (i-1) (j+1) n

{-
in my mac: /Users/mamtajakter/Documents/drpproject/hblas/.stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/cg
in apollo: /home/users/makter2/drpproject/hblas/.stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/cg
update :: ( Num n)=> [n]-> Int-> n-> [n]
update (x:xs) index value
       | index>0       = x : (update xs (index-1) value)
       | otherwise = value : xs
-}

-- test-suite test_level2
--  ghc-options:         -O2
--                       -dumpdir dumpDirectory/
--                       -ddump-llvm
--                       -ddump-asm
--                       -ddump-to-file
--  type:                exitcode-stdio-1.0
--  default-language:    Haskell2010
--  main-is:             tests/testLevel2.hs
--  build-depends:       base,
--                       vector,
--                       hblas,
--                       parallel,
--                       time >= 1.6
