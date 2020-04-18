module HBLAS.Generic where

{- TODO:

   * remove length checking operations, with what BLAS does. 'length' is O(n) so
     this is a major source of inefficiency

   * turn flags represented as floats to flags represented as Bools

-}

import qualified Data.List as List
import qualified Data.Vector as Vec
import Prelude hiding (zipWith,(!!))
import Data.Foldable
import Data.Monoid
import Data.Time

class Foldable f => Zippable f where
  zipWith :: (a -> b -> c) -> f a -> f b -> f c

instance Zippable [] where
  zipWith = List.zipWith

instance Zippable Vec.Vector where
  zipWith = Vec.zipWith

class Indexable f where
  (!!) :: f a -> Int -> a
  elemIndex :: (Eq a) => a -> f a -> Maybe Int

instance Indexable [] where
  (!!) = (List.!!)
  elemIndex = List.elemIndex

instance Indexable Vec.Vector where
  (!!) = (Vec.!)
  elemIndex = Vec.elemIndex

--Left
drotg :: (Floating n, Ord n) => n -> n -> n -> n -> (n,n)
drotg a b c s = (drotg_r a b c s, drotg_z a b c s)

drotg_roe :: (Num n,Ord n) => n -> n -> n
drotg_roe a b =
  case abs a > abs b of
    True -> a
    False -> b

drotg_scale :: Num n => n -> n -> n
drotg_scale a b = abs a + abs b

drotg_r :: (Num n, Ord n, Floating n) => n -> n -> n -> n ->n
drotg_r a b c s | scale == 0 =0.0
                | otherwise  = (signum (roe))* (roe) * scale * sqrt (a/scale)*(a/scale)+(b/scale)*(b/scale)
                where scale=drotg_scale a b
                      roe=drotg_roe a b

drotg_z :: (Ord n, Floating n) => n -> n -> n -> n -> n
drotg_z a b c s | abs a>abs b                 = z_final
                | abs b>=abs a && c_final /=0 = 1/ c_final
                where c_final=a/(drotg_r a b c s)
                      z_final=b/(drotg_r a b c s)

--Done
drot :: (Zippable f, Num n) => f n -> f n -> n -> n -> (f n, f n)
drot x y c s = (drot1 x y c s, drot2 x y c s )

drot1 :: (Zippable f, Num n) => f n -> f n -> n-> n-> f n
drot1 x y c s = zipWith (\x y -> c*x + s*y) x y

drot2 :: (Zippable f, Num n) => f n -> f n -> n -> n -> f n
drot2 x y c s = zipWith (\x y -> c*y - s*x) x y

--Done
drotm :: (Zippable f, Indexable f, Num n, Eq n, Ord n)
      => f n -> f n -> f n -> (f n, f n)
drotm x y dparam | (dparam !! 0) <0 = (zipWith (\x y -> x*(dparam !! 1) + y*(dparam !! 3)) x y, zipWith (\y x -> x*(dparam !! 2) + y*(dparam !! 4)) y x)
                | (dparam !! 0) ==0 = (zipWith (\x y -> x + y*(dparam !! 3)) x y, zipWith (\y x -> x*(dparam !! 2) + y) y x)
                | otherwise = (zipWith (\x y -> x*(dparam !! 1) + y) x y, zipWith (\y x ->  y*(dparam !! 4)-x) y x)

--Done
dswap :: (a, b) -> (b, a)
dswap (x, y) = (y, x)

--Done
dscal :: (Functor f, Num n) => n -> f n -> f n
dscal alpha = fmap (alpha*)

 --Done
dcopy :: (Foldable f, Num n) => f n -> f n -> f n
dcopy xs ys = xs


--Done
daxpy :: (Zippable f, Num n) => n -> f n -> f n -> f n
daxpy alpha xs ys =zipWith (\x y -> y+alpha*x) xs ys
 
--Done   
ddot :: (Zippable f, Num n) => f n -> f n -> n
ddot a b =sum (zipWith (*) a b)
         
--Done
dnrm2 :: (Foldable f, Functor f, Num n, Floating n) => f n -> n
dnrm2 = sqrt . sum . fmap (\i -> i * i)


--Left
dznrm2 :: (Foldable f, Eq (f n), Monoid (f n), Num n, Floating n, Ord n)
       => f n -> n
dznrm2 x =let (ssq,scale) = foldr (\e (s,c) -> if c<(abs e)
                                                  then (1+s*(c/ (abs e))*(c/ (abs e)), abs e)
                                                  else (s+((abs e)/c)*((abs e)/c), c)) (1,0)  x
             in scale * (sqrt ssq)


--Done
dasum :: (Foldable f, Num n) => f n -> n
dasum = foldr (\x a -> abs x + a) 0

--Done
idamax :: (Indexable f, Functor f, Foldable f, Eq (f n), Monoid (f n), Num n, Ord n)
       => f n -> Maybe Int
idamax a = elemIndex (maximum (fmap abs a)) (fmap abs a)

gam,gamsq,rgamsq :: Floating n => n
gam=4096.0
gamsq=16777216.0
rgamsq=0.000000059604645

f_2_4:: (Num n, Ord n) => n -> n -> n -> n -> n -> n ->Bool
f_2_4 d1 x1 d2 y1 h12 h21 = (abs (d1*x1*x1)> abs (d2*y1*y1)) && (1-h12*h21)>0

f_2_else_5_if :: (Num n, Ord n) => n -> n -> n -> n -> Bool
f_2_else_5_if d1 x1 d2 y1 = (abs (d1*x1*x1)<= abs (d2*y1*y1)) && (d2*y1*y1)<0

f_2_else_5_else :: (Num n, Ord n) => n -> n -> n -> n -> Bool
f_2_else_5_else d1 x1 d2 y1 = (abs (d1*x1*x1)<= abs (d2*y1*y1)) && (d2*y1*y1)>=0

f_6_8_if :: (Floating n, Ord n) => n -> Bool
f_6_8_if d1 = d1 /=0 && ((d1<=rgamsq) || (d1>=gamsq)) && (d1<rgamsq)

f_6_8_else :: (Floating n, Ord n) => n -> Bool
f_6_8_else d1 = d1 /=0 && ((d1<=rgamsq) || (d1>=gamsq)) && (d1>=rgamsq)

f_6_7_if :: (Floating n, Ord n) => n -> Bool -> Bool
f_6_7_if d1 flag = d1 /=0 && ((d1<=rgamsq) || (d1>=gamsq)) && not (flag)

f_6_7_else :: (Floating n, Ord n) => n -> Bool -> Bool
f_6_7_else d1 flag = d1 /=0 && ((d1<=rgamsq) || (d1>=gamsq)) && flag

f_9_10_if :: (Floating n, Ord n) => n -> Bool -> Bool
f_9_10_if d2 flag = (d2 /=0) && ((abs d2 <= rgamsq) || (abs d2>=gamsq) )  && not (flag)

f_9_10_else :: (Floating n, Ord n) =>  n -> Bool -> Bool
f_9_10_else d2 flag | (d2 /=0) && ((abs d2 <= rgamsq) || (abs d2>=gamsq) )  && flag = True
                    | otherwise                                                     = False

f_9_11_if :: (Floating n, Ord n) => n -> Bool
f_9_11_if d2 | d2 /=0 && ((d2<=rgamsq) || (d2>=gamsq)) && (d2<rgamsq) = True
             | otherwise                                              = False

f_9_11_else :: (Floating n, Ord n) => n -> Bool
f_9_11_else d2 | d2 /=0 && ((d2<=rgamsq) || (d2>=gamsq)) && (d2>=rgamsq) = True
               | otherwise                                              = False

drotmg_d1 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_d1  d1 d2 x1 y1 flag h11 h12 h21 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21   = d1/(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1   = 0
                                               | f_2_else_5_else d1 x1 d2 y1  =  d2/(1+(h11*h22))
                                               | f_6_8_if d1                 =  d1*(gam*gam)
                                               | f_6_8_else d1               =  d1/(gam*gam)
                                               | otherwise =0

drotmg_d2 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_d2  d1 d2 x1 y1 flag h11 h12 h21 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21  = d2/(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1  = 0
                                               | f_2_else_5_else d1 x1 d2 y1   =  d1/(1+(h11*h22))
                                               | f_9_11_if d2 =  d2*(gam*gam)
                                               | f_9_11_else d2 =  d2/(gam*gam)
                                               | otherwise =0

drotmg_x1 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_x1  d1 d2 x1 y1 flag h11 h12 h21 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21 = x1*(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1  = 0
                                               | f_2_else_5_else d1 x1 d2 y1 =  y1*(1+(h11*h22))
                                               | f_6_8_if d1 =  x1/gam
                                               | f_6_8_else d1 =  x1*gam
                                               | otherwise = 0

drotmg_flag :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_flag  d1 d2 x1 y1 flag h11 h12 h21 h22  | d1<0           = negate 1--1
                                               | d2*y1==0      =negate 2--3
                                               | f_2_4 d1 x1 d2 y1 h12 h21 = 0--2-4
                                               | f_2_else_5_if d1 x1 d2 y1  = negate 1-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1   =  1-- 2-else-5-else
                                               | f_6_7_if d1 flag = negate 1-- 6-7-if
                                               | f_9_10_if d2 flag = negate 1 ----9-10-if
                                               | otherwise = negate 1


drotmg_h11 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_h11  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1  =  (d1*x1)/(d2*y1)-- 2-else-5-else
                                               | f_6_7_if d1 flag =  1-- 6-7-if
                                               | f_6_8_if d1 =  h11/gam-- 6-8-if
                                               | f_6_8_else d1 =  h11*gam -- 6-8-else
                                               | f_9_10_if d2 flag = 1 ----9-10-if
                                               | otherwise = 0


drotmg_h12 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_h12  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | abs (d1*x1*x1)> abs (d2*y1*y1)  = (d2*y1)/(d1*x1)-- 2-if
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_6_7_else d1 flag =  1-- 6-7-else
                                               | f_6_8_if d1 =  h12/gam-- 6-8-if
                                               | f_6_8_else d1 =  h12*gam -- 6-8-else
                                               | f_9_10_else d2 flag = 1 ----9-10-else
                                               | otherwise = 0


drotmg_h21 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_h21  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | abs (d1*x1*x1)> abs (d2*y1*y1)  = negate (y1/x1)-- 2-if
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_6_7_else d1 flag = negate 1-- 6-7-else
                                               | f_9_10_else d2 flag = negate 1 ----9-10-else
                                               | f_9_11_if d2 =  h21/gam-- 9-11-if
                                               | f_9_11_else d2 =  h21*gam -- 9-11-else
                                               | otherwise = 0

drotmg_h22 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_h22  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | f_2_else_5_if d1 x1 d2 y1 = 0-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1  =  x1/y1-- 2-else-5-else
                                               | f_6_7_if d1 flag =  1-- 6-7-if
                                               | f_9_10_if d2 flag = 1 ----9-10-if
                                               | f_9_11_if d2 =  h22/gam-- 9-11-if
                                               | f_9_11_else d2 =  h22*gam -- 9-11-else
                                               | otherwise = 0

drotmg :: (Applicative f, Indexable f, Monoid (f n), Floating n, Ord n)
       => n -> n -> n -> n -> f n -> (n,n,n,f n)
drotmg d1 d2 x1 y1 dparam=
    let flag= if (dparam !! 0) >0 then True else False
        h11=dparam !! 1
        h21= dparam !! 2
        h12=dparam !! 3
        h22=dparam !! 4
    in (drotmg_d1 d1 d2 x1 y1 flag h11 h21 h12 h22,drotmg_d2 d1 d2 x1 y1 flag h11 h21 h12 h22,drotmg_x1 d1 d2 x1 y1 flag h11 h21 h12 h22, pure (drotmg_flag d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (drotmg_h11 d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (drotmg_h21 d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (drotmg_h12 d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (drotmg_h22 d1 d2 x1 y1 flag h11 h21 h12 h22))


main = double >> single >> doubleVec >> singleVec
  where double :: IO ()
        double =
          do let c,s,alpha,a,m,n,d1,d2,x1,x2 :: Double
                 x,y,param :: [Double]
                 x = [1..5]
                 y = [1..5]
                 param = take 5 (repeat 2)
                 c = 1.0
                 s=2.0
                 alpha=3.0
                 a=5.0
                 m=8.0
                 n= 9.0
                 d1=1000000000.0
                 d2=1.0
                 x1=2.0
                 x2=4.0
             putStrLn "Double Precision List"
             t <- getCurrentTime
             print (drotg m n c s)
             print (drotmg d1 d2 x1 x2 param :: (Double,Double,Double,[Double]))
             print (drot x y c s)
             print (drotm x y param)
             print (dswap (x,y))
             print (dscal alpha x )--[3.0,9.0,-15.0]
             print (dcopy x y)--[-1.0,-2.0,4.0]
             print (daxpy a x y)--[ 6, 11, 16, 21, 26 ]
             print (ddot x y)--- 3.0
             print (dnrm2 x)--3.74
             print (dznrm2 x)
             print (dasum x)--19.0
             print (idamax x)-- Just 4
             t' <- getCurrentTime
             putStrLn $ "time " ++ (show (diffUTCTime t' t))


        single :: IO ()
        single =
          do let c,s,alpha,a,m,n,d1,d2,x1,x2 :: Float
                 x,y,param :: [Float]
                 x = [1..5]
                 y = [1..5]
                 param = take 5 (repeat 2)
                 c = 1.0
                 s=2.0
                 alpha=3.0
                 a=5.0
                 m=8.0
                 n= 9.0
                 d1=1000000000.0
                 d2=1.0
                 x1=2.0
                 x2=4.0
             putStrLn "\nSingle Precision List"
             t <- getCurrentTime
             print (drotg m n c s :: (Float,Float))
             print (drotmg d1 d2 x1 x2 param :: (Float, Float, Float, [Float]))
             print (drot x y c s)
             print (drotm x y param)
             print (dswap (x,y))
             print (dscal alpha x )--[3.0,9.0,-15.0]
             print (dcopy x y)--[-1.0,-2.0,4.0]
             print (daxpy a x y)--[ 6, 11, 16, 21, 26 ]
             print (ddot x y)--- 3.0
             print (dnrm2 x)--3.74
             print (dznrm2 x)
             print (dasum x)--19.0
             print (idamax x)-- Just 4
             t' <- getCurrentTime
             putStrLn $ "time " ++ (show (diffUTCTime t' t))


        doubleVec :: IO ()
        doubleVec =
          do let c,s,alpha,a,m,n,d1,d2,x1,x2 :: Double
                 x,y,param :: Vec.Vector Double
                 x = Vec.fromList [1..5]
                 y = Vec.fromList [1..5]
                 param = Vec.fromList (take 5 . repeat $ 2)
                 c = 1.0
                 s=2.0
                 alpha=3.0
                 a=5.0
                 m=8.0
                 n= 9.0
                 d1=1000000000.0
                 d2=1.0
                 x1=2.0
                 x2=4.0
             putStrLn "\nDouble Precision Vector"
             t <- getCurrentTime
             print (drotg m n c s)
             print (drotmg d1 d2 x1 x2 param)
             print (drot x y c s)
             print (drotm x y param)
             print (dswap (x,y))
             print (dscal alpha x )--[3.0,9.0,-15.0]
             print (dcopy x y)--[-1.0,-2.0,4.0]
             print (daxpy a x y)--[ 6, 11, 16, 21, 26 ]
             print (ddot x y)--- 3.0
             print (dnrm2 x)--3.74
             print (dznrm2 x)
             print (dasum x)--19.0
             print (idamax x)-- Just 4
             t' <- getCurrentTime
             putStrLn $ "time " ++ (show (diffUTCTime t' t))


        singleVec :: IO ()
        singleVec =
          do let c,s,alpha,a,m,n,d1,d2,x1,x2 :: Float
                 x,y,param :: Vec.Vector Float
                 x = Vec.fromList [1..5]
                 y = Vec.fromList [1..5]
                 param = Vec.fromList (take 5 . repeat $ 2)
                 c = 1.0
                 s=2.0
                 alpha=3.0
                 a=5.0
                 m=8.0
                 n= 9.0
                 d1=1000000000.0
                 d2=1.0
                 x1=2.0
                 x2=4.0
             putStrLn "\nSingle Precision Vector"
             t <- getCurrentTime
             print (drotg m n c s)
             print (drotmg d1 d2 x1 x2 param)
             print (drot x y c s)
             print (drotm x y param)
             print (dswap (x,y))
             print (dscal alpha x )--[3.0,9.0,-15.0]
             print (dcopy x y)--[-1.0,-2.0,4.0]
             print (daxpy a x y)--[ 6, 11, 16, 21, 26 ]
             print (ddot x y)--- 3.0
             print (dnrm2 x)--3.74
             print (dznrm2 x)
             print (dasum x)--19.0
             print (idamax x)-- Just 4
             t' <- getCurrentTime
             putStrLn $ "time " ++ (show (diffUTCTime t' t))



--Mamtajs-MacBook-Pro:~ mamtajakter$ cd desktop/hblas/src
--Mamtajs-MacBook-Pro:haskell mamtajakter$ ghc -O2 --make hblas_generic_optimized.hs
--Mamtajs-MacBook-Pro:haskell mamtajakter$ ./hblas

--Mamtajs-MacBook-Pro:haskell mamtajakter$ ghc -fllvm -keep-llvm-files -fforce-recomp hblas.hs
