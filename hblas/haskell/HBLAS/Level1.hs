{-# LANGUAGE BangPatterns #-}
module HBLAS.Level1 where


import Prelude hiding (map,zipWith,(!!))
import Data.Monoid
import qualified Data.Vector as V
import Control.Applicative

--Done
{-# INLINE scal #-}
scal :: ( Num n) => n -> V.Vector n -> V.Vector n
scal !alpha !x= fmap (alpha*) x

--Done
{-# INLINE axpy #-}
axpy :: ( Num n) => n -> V.Vector n -> V.Vector n -> V.Vector n
axpy !alpha !xs !ys =V.zipWith (\x y -> y+alpha*x) xs ys

--Done
{-# INLINE dot #-}
dot :: ( Num n) => V.Vector n -> V.Vector n -> n
dot !a !b = sum (V.zipWith (*) a b)

{-# INLINE nrm2 #-}
nrm2 :: (  Num n, Floating n) => V.Vector n -> n
nrm2 = sqrt . sum . fmap (\i -> i * i)

--Matched
{-# INLINE rotg #-}
rotg :: (Floating n, Ord n) => n -> n  -> (n,n,n,n)
rotg !a !b  = (rotg_r a b , rotg_z a b ,rotg_c a b , rotg_s a b  )

{-# INLINE rotg_roe #-}
rotg_roe :: (Num n,Ord n) => n -> n -> n
rotg_roe !a !b =
  case abs a > abs b of
    True -> a
    False -> b

{-# INLINE rotg_r #-}
rotg_r :: (Num n, Ord n, Floating n) => n -> n  ->n
rotg_r !a !b  = (signum (roe)) * sqrt (a*a+b*b)
                where !roe=rotg_roe a b

{-# INLINE rotg_z #-}
rotg_z :: (Ord n, Floating n) => n -> n -> n
rotg_z !a !b  | abs a>abs b = rotg_s a b
            | (abs b>=abs a) && ((rotg_c a b ) /=0) && (rotg_r a b ) /=0 = 1/ rotg_c a b
            | (abs b>=abs a) && ((rotg_c a b ) ==0)  && (rotg_r a b ) /=0 = 1
            | (rotg_r a b ) ==0 =0
            | otherwise = 0

{-# INLINE rotg_c #-}
rotg_c :: (Ord n, Floating n) => n -> n-> n
rotg_c !a !b =case (rotg_r a b ) of
                        0  -> 1
                        _  -> a/(rotg_r a b )

{-# INLINE rotg_s #-}
rotg_s :: (Ord n, Floating n) => n -> n  -> n
rotg_s !a !b  =case (rotg_r a b ) of
                        0  -> 0
                        _  -> b/(rotg_r a b )

--Matched
{-# INLINE rot #-}
rot :: (Floating n, Eq n, Num n) => V.Vector n -> V.Vector n -> n  -> (V.Vector n, V.Vector n)
rot !x !y !c
         | c==1      = (x,y)
         | otherwise = let !s= sqrt (3)*c in
                       (V.zipWith (\w z -> (c*w) + (s*z)) x y, V.zipWith (\w z -> (c*z) - (s*w)) x y )

--Matched
{-# INLINE rotm #-}
rotm :: ( Num n, Eq n, Ord n)
      => V.Vector n -> V.Vector n -> V.Vector n -> (V.Vector n, V.Vector n)
rotm !x !y !dparam= do
                  let !(h11,h12,h21,h22)=hrotm dparam
                  (V.zipWith (\w z -> w*h11 + z*h12) x y, V.zipWith (\w z -> w*h21 + z*h22) y x)

{-# INLINE hrotm #-}
hrotm ::( Eq n, Num n)=> V.Vector n -> (n,n,n,n)
hrotm !dparam1= case dparam1 V.! 0 of
                  0  -> (1,h12,h21,1)
                  1  -> (h11,1,(negate 1),h22)
                  -2 -> (1,0,0,1)
                  _  -> (h11,h12,h21,h22)
                  where !h11  =dparam1 V.! 1
                        !h12  = dparam1 V.! 2
                        !h21 = dparam1 V.! 3
                        !h22= dparam1 V.! 4

--Done
{-# INLINE swap #-}
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

--Done
{-# INLINE copy #-}
copy :: ( Eq n, Num n) => V.Vector n -> V.Vector n -> V.Vector n
copy xs _ = xs

{-# INLINE znrm2 #-}
znrm2 :: (  Num n, Floating n, Ord n)
       => V.Vector n -> n
znrm2 !x =let (ssq,scale) = foldr (\e (s,c) -> case c<(abs e) of
                                                  True  -> (1+s*(c/ (abs e))*(c/ (abs e)), abs e)
                                                  False -> (s+((abs e)/c)*((abs e)/c), c)) (1,0)  x
             in scale * (sqrt ssq)

--Done
{-# INLINE asum #-}
asum :: ( Num n) => V.Vector n -> n
asum !y= foldr (\x a -> abs x + a) 0 y

--Done
{-# INLINE idamax #-}
idamax :: (Num n, Ord n)
       => V.Vector n -> Maybe Int
idamax !a = V.elemIndex (maximum (fmap abs a)) (fmap abs a)

{-# INLINE idamin #-}
idamin :: (Num n, Ord n)
       => V.Vector n -> Maybe Int
idamin !a = V.elemIndex (minimum (fmap abs a)) (fmap abs a)

{-# INLINE axpyaxpy #-}
axpyaxpy ::  ( Num n) => n -> n -> V.Vector n -> V.Vector n -> V.Vector n
axpyaxpy !a1 !a2 !x !y  = let !t1 = axpy a1 x y in axpy a2 t1 x

{-# INLINE axpy' #-}
axpy' :: ( Num n) => n -> V.Vector n -> V.Vector n -> V.Vector n-> V.Vector n
axpy' !alpha !x !y !vec0 =let x1 = axpy alpha x vec0 in axpy 1 x1 y

-- test-suite test_level1
--   ghc-options:         -O2
--                        -dumpdir dumpDirectory/
--                        -ddump-llvm
--                        -ddump-asm
--                        -ddump-to-file
--   type:                exitcode-stdio-1.0
--   default-language:    Haskell2010
--   main-is:             tests/testLevel1.hs
--   build-depends:       base,
--                        vector,
--                        containers,
--                        hblas,
--                        time >= 1.6


-- test-suite level1
--   ghc-options:         -O2
--   type:                exitcode-stdio-1.0
--   default-language:    Haskell2010
--   main-is:             tests/testLevel1.hs
--   build-depends:       base,
--                        vector,
--                        containers,
--                        hblas,
--                        time >= 1.6
