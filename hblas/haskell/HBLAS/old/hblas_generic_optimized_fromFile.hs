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

--Matched
drotg :: (Floating n, Ord n) => n -> n  -> (n,n,n,n)
drotg a b  = (drotg_r a b , drotg_z a b ,drotg_c a b , drotg_s a b  )

drotg_roe :: (Num n,Ord n) => n -> n -> n
drotg_roe a b =
  case abs a > abs b of
    True -> a
    False -> b

drotg_r :: (Num n, Ord n, Floating n) => n -> n  ->n
drotg_r a b  = (signum (roe)) * sqrt (a*a+b*b)
                where roe=drotg_roe a b

drotg_z :: (Ord n, Floating n) => n -> n -> n
drotg_z a b  | abs a>abs b                                              = drotg_s a b 
                | (abs b>=abs a) && ((drotg_c a b ) /=0) && (drotg_r a b ) /=0 = 1/ drotg_c a b 
                | (abs b>=abs a) && ((drotg_c a b ) ==0)  && (drotg_r a b ) /=0 = 1
                | (drotg_r a b ) ==0                                    =0      

drotg_c :: (Ord n, Floating n) => n -> n-> n
drotg_c a b =case (drotg_r a b ) of 
                        0  -> 1
                        _  -> a/(drotg_r a b )

drotg_s :: (Ord n, Floating n) => n -> n  -> n 
drotg_s a b  =case (drotg_r a b ) of 
                        0  -> 0
                        _  -> b/(drotg_r a b )

--Matched
drot :: (Floating n,Zippable f, Eq n, Num n) => f n -> f n -> n  -> (f n, f n)
drot x y c 
         | c==1      = (x,y) 
         | otherwise = let s= sqrt (3)*c in
                       (zipWith (\x y -> (c*x) + (s*y)) x y, zipWith (\x y -> (c*y) - (s*x)) x y )

-- --Matched
drotm :: (Zippable f, Indexable f, Num n, Eq n, Ord n)
      => f n -> f n -> f n -> (f n, f n)
drotm x y dparam= do
                  let (h11,h12,h21,h22)=hdrotm dparam
                  (zipWith (\x y -> x*h11 + y*h12) x y, zipWith (\y x -> x*h21 + y*h22) y x)

hdrotm ::(Foldable f, Indexable f, Eq n, Num n)=> f n -> (n,n,n,n)                  
hdrotm dparam1= case dparam1!!0 of 
                  0  -> (1,h12,h21,1)
                  1  -> (h11,1,(negate 1),h22)
                  -2 -> (1,0,0,1)
                  _  -> (h11,h12,h21,h22) 
                  where h11  =dparam1 !! 1
                        h12  = dparam1!! 2
                        h21 = dparam1!! 3
                        h22= dparam1 !! 4
-- drotm :: (Zippable f, Indexable f, Num n, Eq n, Ord n)
--       => f n -> f n -> f n -> (f n, f n)
-- drotm x y dparam | (dparam !! 0) <0 = (zipWith (\x y -> x*(dparam !! 1) + y*(dparam !! 3)) x y, zipWith (\y x -> x*(dparam !! 2) + y*(dparam !! 4)) y x)
--                 | (dparam !! 0) ==0 = (zipWith (\x y -> x + y*(dparam !! 3)) x y, zipWith (\y x -> x*(dparam !! 2) + y) y x)
--                 | otherwise = (zipWith (\x y -> x*(dparam !! 1) + y) x y, zipWith (\y x ->  y*(dparam !! 4)-x) y x)

--Done
dswap :: (a, b) -> (b, a)
dswap (x, y) = (y, x)

--Done
dscal :: (Functor f, Num n) => n -> f n -> f n
dscal alpha = fmap (alpha*)

 --Done
dcopy :: (Foldable f, Eq n, Num n) => f n -> f n -> f n
dcopy xs ys = xs


--Done
daxpy :: (Zippable f, Num n) => n -> f n -> f n -> f n
daxpy alpha xs ys =zipWith (\x y -> y+alpha*x) xs ys

--Done
ddot :: (Zippable f, Num n) => f n -> f n -> n
ddot a b =sum (zipWith (*) a b)

--Done matched
dnrm2 :: (Foldable f, Functor f, Num n, Floating n) => f n -> n
dnrm2 = sqrt . sum . fmap (\i -> i * i)


--Left
dznrm2 :: (Foldable f, Eq (f n), Monoid (f n), Num n, Floating n, Ord n)
       => f n -> n
dznrm2 x =let (ssq,scale) = foldr (\e (s,c) -> case c<(abs e) of
                                                  True  -> (1+s*(c/ (abs e))*(c/ (abs e)), abs e)
                                                  False -> (s+((abs e)/c)*((abs e)/c), c)) (1,0)  x
             in scale * (sqrt ssq)

--Done
dasum :: (Foldable f, Num n) => f n -> n
dasum = foldr (\x a -> abs x + a) 0

--Done
idamax :: (Indexable f, Functor f, Foldable f, Eq (f n), Monoid (f n), Num n, Ord n)
       => f n -> Maybe Int
idamax a = elemIndex (maximum (fmap abs a)) (fmap abs a)

idamin :: (Indexable f, Functor f, Foldable f, Eq (f n), Monoid (f n), Num n, Ord n)
       => f n -> Maybe Int
idamin a = elemIndex (minimum (fmap abs a)) (fmap abs a)

-- some matches, some doesnt
--59.6046,1.0000,-8192.0000,[-1.0000,4096.0000,2.0000,-0.0000,1.0000]
--(59.604645,5.9604645e-8,-8192.0,[-1.0,8192.0,2.0,-2.0e-9,8192.0])

gam,gamsq,rgamsq :: Floating n => n
gam=4096.0
gamsq=16777200.0
rgamsq=negate 31.4814698927

f_2_4:: (Num n, Ord n) => n -> n -> n -> n -> n -> n ->Bool
f_2_4 d1 x1 d2 y1 h12 h21 
                       | (abs (d1*x1*x1)> abs (d2*y1*y1)) && (1-h12*h21)>0 = True
                       | otherwise                                         = False


f_2_else_5_if :: (Num n, Ord n) => n -> n -> n -> n -> Bool
f_2_else_5_if d1 x1 d2 y1 
                       | d1>=0 && (abs (d1*x1*x1)<= abs (d2*y1*y1)) && (d2*y1*y1)<0 = True
                       | otherwise                                                  = False

f_2_else_5_else :: (Num n, Ord n) => n -> n -> n -> n -> Bool
f_2_else_5_else d1 x1 d2 y1 
                       | d1>=0 &&  (abs (d1*x1*x1)<= abs (d2*y1*y1)) && (d2*y1*y1)>=0 = True
                       | otherwise                                                    = False


f_6_8_if :: (Floating n, Ord n) => n -> Bool -> Bool
f_6_8_if d1 flag 
              |  (d1 >0) && (d1<rgamsq)= True
              |  otherwise             = False

f_6_8_else :: (Floating n, Ord n) => n -> Bool -> Bool
f_6_8_else d1 flag 
              |  (d1 >0) && (d1>rgamsq) = True
              | otherwise               = False


f_6_7_if :: (Floating n, Ord n) => n -> Bool -> Bool
f_6_7_if d1 flag 
              |  (d1 >0) && not (flag)= True
              | otherwise             = False

f_6_7_else :: (Floating n, Ord n) => n -> Bool -> Bool
f_6_7_else d1 flag 
              |  (d1 >0) && flag = True
              | otherwise        = False


f_9_10_if :: (Floating n, Ord n) => n ->n-> Bool -> Bool
f_9_10_if d1 d2 flag 
                  |  (d1>=0) &&  (d2 /=0) && not (flag) =True
                  |  otherwise                          =False     

f_9_10_else :: (Floating n, Ord n) =>  n ->n-> Bool -> Bool
f_9_10_else d1 d2 flag 
                    | (d1>=0) &&  (d2 /=0) && flag = True
                    | otherwise                    = False

f_9_11_if :: (Floating n, Ord n) => n->n -> Bool
f_9_11_if d1 d2 | (d1>=0) && (d2 /=0) && ((abs d2)<=rgamsq) = True
                | otherwise                                 = False

f_9_11_else :: (Floating n, Ord n) => n->n -> Bool
f_9_11_else d1 d2 | (d1>=0) && (d2 /=0) && ((abs d2)>rgamsq) = True
                  | otherwise                                = False

drotmg_d1 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_d1  d1 d2 x1 y1 flag h11 h21 h12 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21   = d1/(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1   = 0
                                               | f_2_else_5_else d1 x1 d2 y1  =  d2/(1+(h11*h22))
                                               | f_6_8_if d1 flag               =  d1*(gam*gam)
                                               | f_6_8_else d1 flag           =  d1/(gam*gam)
                                               | otherwise =0

drotmg_d2 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_d2  d1 d2 x1 y1 flag h11 h21 h12 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21  = d2/(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1  = 0
                                               | f_2_else_5_else d1 x1 d2 y1   =  d1/(1+(h11*h22))
                                               | f_9_11_if d1 d2 =  d2*(gam*gam)
                                               | f_9_11_else d1 d2 =  d2/(gam*gam)
                                               | otherwise =0

drotmg_x1 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_x1  d1 d2 x1 y1 flag h11 h21 h12 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21 = x1*(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1  = 0
                                               | f_2_else_5_else d1 x1 d2 y1 =  y1*(1+(h11*h22))
                                               | f_6_8_if d1 flag=  x1/gam
                                               | f_6_8_else d1 flag =  x1*gam
                                               | otherwise = 0

drotmg_flag :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_flag  d1 d2 x1 y1 flag h11 h21 h12 h22  | d1<0           = negate 1--1
                                               | d2*y1==0      =negate 2--3
                                               | f_2_4 d1 x1 d2 y1 h12 h21 = 0--2-4
                                               | f_2_else_5_if d1 x1 d2 y1  = negate 1-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1   =  1-- 2-else-5-else
                                               | f_6_7_if d1 flag = negate 1-- 6-7-if
                                               | f_9_10_if d1 d2 flag = negate 1 ----9-10-if
                                               | otherwise = negate 1


drotmg_h11 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_h11  d1 d2 x1 y1 flag h11 h21 h12 h22   | d1<0           = 0--1
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1  =  (d1*x1)/(d2*y1)-- 2-else-5-else
                                               | f_6_7_if d1 flag =  1-- 6-7-if
                                               | f_6_8_if d1 flag =  h11/gam-- 6-8-if
                                               | f_6_8_else d1 flag =  h11*gam -- 6-8-else
                                               | f_9_10_if d1 d2 flag = 1 ----9-10-if
                                               | otherwise = 0


drotmg_h12 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_h12  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | abs (d1*x1*x1)> abs (d2*y1*y1)  = (d2*y1)/(d1*x1)-- 2-if
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_6_7_else d1 flag =  1-- 6-7-else
                                               | f_6_8_if d1 flag=  h12/gam-- 6-8-if
                                               | f_6_8_else d1 flag =  h12*gam -- 6-8-else
                                               | f_9_10_else d1 d2 flag = 1 ----9-10-else
                                               | otherwise = 0


drotmg_h21 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_h21  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | abs (d1*x1*x1)> abs (d2*y1*y1)  = negate (y1/x1)-- 2-if
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_6_7_else d1 flag = negate 1-- 6-7-else
                                               | f_9_10_else d1 d2 flag = negate 1 ----9-10-else
                                               | f_9_11_if d1 d2 =  h21/gam-- 9-11-if
                                               | f_9_11_else d1 d2 =  h21*gam -- 9-11-else
                                               | otherwise = 0

drotmg_h22 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
drotmg_h22  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | f_2_else_5_if d1 x1 d2 y1 = 0-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1  =  x1/y1-- 2-else-5-else
                                               | f_6_7_if d1 flag =  1-- 6-7-if
                                               | f_9_10_if d1 d2 flag = 1 ----9-10-if
                                               | f_9_11_if d1 d2 =  h22/gam-- 9-11-if
                                               | f_9_11_else d1 d2 =  h22*gam -- 9-11-else
                                               | otherwise = 0

drotmg :: (Applicative f, Indexable f, Monoid (f n), Floating n, Ord n)
       => n -> n -> n -> n -> f n -> (n,n,n,f n)
drotmg d1 d2 x1 y1 dparam=
    let flag= if (dparam !! 0) >0 then True else False
        h11=dparam !! 1
        h21= dparam !! 2
        h12=dparam !! 3
        h22=dparam !! 4
    in (drotmg_d1 d1 d2 x1 y1 flag h11 h21 h12 h22,drotmg_d2 d1 d2 x1 y1 flag h11 h21 h12 h22,drotmg_x1 d1 d2 x1 y1 flag h11 h21 h12 h22, pure (drotmg_flag d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (drotmg_h11 d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (drotmg_h12 d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (drotmg_h21 d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (drotmg_h22 d1 d2 x1 y1 flag h11 h21 h12 h22))


vecFromFile :: (Floating n, Read n)
            => FilePath
            -> IO (Vec.Vector n)
vecFromFile f = fmap read . Vec.fromList . lines <$> readFile f

listFromFile :: (Floating n, Read n)
            => FilePath
            -> IO [n]
listFromFile f = fmap read . lines <$> readFile f


mains :: IO ()
mains = double >> single >> doubleVec >> singleVec
  where double :: IO ()
        double =
          do x <- listFromFile "vec1.csv"
             y <- listFromFile "vec2.csv"
             let c,s,alpha,a,m,n,d1,d2,x1,x2 :: Double
                 param :: [Double]
                 param = take 5 (repeat 2)
                 c = 0.5
                 s=2.0
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
             print (drotg m n )
             print (drotmg d1 d2 x1 x2 param :: (Double,Double,Double,[Double]))
             print (drot x y c)
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
             print (idamin x)-- Just 4
             t' <- getCurrentTime
             putStrLn $ "time " ++ (show (diffUTCTime t' t))


        single :: IO ()
        single =
          do x <- listFromFile "vec1.csv"
             y <- listFromFile "vec2.csv"
             let c,s,alpha,a,m,n,d1,d2,x1,x2 :: Float
                 param :: [Float]
                 param = take 5 (repeat 2)
                 c = 0.5
                 s=2.0
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
             print (drotg m n  :: (Float,Float,Float,Float))
             print (drotmg d1 d2 x1 x2 param :: (Float, Float, Float, [Float]))
             print (drot x y c)
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
             print (idamin x)-- Just 4
             t' <- getCurrentTime
             putStrLn $ "time " ++ (show (diffUTCTime t' t))


        doubleVec :: IO ()
        doubleVec =
          do x<- vecFromFile "vec1.csv"
             y<- vecFromFile "vec2.csv"
             let c,s,alpha,a,m,n,d1,d2,x1,x2 :: Double
                 param :: Vec.Vector Double
                 param = Vec.fromList (take 5 . repeat $ 2)
                 c = 0.5
                 s=2.0
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
             print (drotg m n )
             print (drotmg d1 d2 x1 x2 param)
             print (drot x y c)
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
             print (idamin x)-- Just 4
             t' <- getCurrentTime
             putStrLn $ "time " ++ (show (diffUTCTime t' t))


        singleVec :: IO ()
        singleVec =
          do x <- vecFromFile "vec1.csv"
             y <- vecFromFile "vec2.csv"
             let c,s,alpha,a,m,n,d1,d2,x1,x2 :: Float
                 param :: Vec.Vector Float
                 param = Vec.fromList (take 5 . repeat $ 2)
                 c = 0.5
                 s=2.0
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
             print (drotg m n )
             print (drotmg d1 d2 x1 x2 param)
             print (drot x y c)
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
             print (idamin x)-- Just 4
             t' <- getCurrentTime
             putStrLn $ "time " ++ (show (diffUTCTime t' t))

--Mamtajs-MacBook-Pro:~ mamtajakter$ cd desktop/hblas/src
--Mamtajs-MacBook-Pro:haskell mamtajakter$ ghc -O2 --make hblas_generic_optimized.hs
--Mamtajs-MacBook-Pro:haskell mamtajakter$ ./hblas
