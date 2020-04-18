--- show psedocode level of code
import Data.List
import Data.Maybe

drotg :: Double-> Double -> Double-> Double -> (Double, Double)
drotg_roe ::  Double->Double->Double
drotg_roe a b | abs(a)>abs(b)= a 
              | otherwise    = b
drotg_scale ::  Double->Double->Double
drotg_scale a b = abs(a)+abs(b)
drotg_r ::  Double->Double->Double->Double->Double
drotg_r a b c s | scale == 0 =0.0 
                | otherwise  = (signum (roe))* (roe) * scale * sqrt (a/scale)*(a/scale)+(b/scale)*(b/scale)
                where scale=drotg_scale a b
                      roe=drotg_roe a b 
drotg_z :: Double->Double->Double-> Double -> Double
drotg_z a b c s | abs a>abs b                 = z_final
                | abs b>=abs a && c_final /=0 = 1/ c_final
                where c_final=a/(drotg_r a b c s)
                      z_final=b/(drotg_r a b c s) 
drotg a b c s = (drotg_r a b c s, drotg_z a b c s)
     
drot :: [Double]-> [Double] -> Double-> Double -> ([Double], [Double])
drot1 ::  [Double]->[Double]->Double->Double->[Double]
drot1 [a] [b] c s=[c*a+s*b]
drot1 x y c s =zipWith (\x y -> c*x + s*y) x y
drot2 ::  [Double]->[Double]->Double->Double->[Double]
drot2 [a] [b] c s=[c*b-s*a]
drot2 x y c s =zipWith (\x y -> c*y - s*x) x y
drot x y c s = (drot1 x y c s, drot2 x y c s )

drotm :: [Double]-> [Double] -> [Double] -> ([Double], [Double])
drotm x y dparam | (dparam !! 0) <0 = (zipWith (\x y -> x*(dparam !! 1) + y*(dparam !! 3)) x y, zipWith (\y x -> x*(dparam !! 2) + y*(dparam !! 4)) y x)
                | (dparam !! 0) ==0 = (zipWith (\x y -> x + y*(dparam !! 3)) x y, zipWith (\y x -> x*(dparam !! 2) + y) y x)
                | otherwise = (zipWith (\x y -> x*(dparam !! 1) + y) x y, zipWith (\y x ->  y*(dparam !! 4)-x) y x)

dswap :: ([Double], [Double]) -> ([Double], [Double])
dswap ([p],[q])=([q],[p])
dswap (x, y) = (y, x)

dscal ::Double-> [Double] -> [Double]
dscal _ [] = []
dscal alpha (x:xs)  = (alpha*x) : dscal alpha xs
 
 --Copies vector to another vector
dcopy :: [Double] -> [Double] -> [Double]
dcopy [] []               = []
dcopy (x:xs) (y:ys) | length xs == length ys = x : dcopy xs ys 
                    | otherwise              = error "Vector sizes must match"

daxpy ::Double-> [Double] -> [Double]-> [Double]
daxpy _ [] []= []
daxpy alpha (x:xs) (y:ys)  |  length xs == length ys = (y+alpha*x) : daxpy alpha xs ys
                           |  otherwise              = error "Vector sizes must match"

ddot :: [Double] -> [Double] -> Double
ddot a b | length a == length b = sum (zipWith (*) a b)
         | otherwise = error "Vector sizes must match"

--Euclidean norm of a vector
dnrm2 :: [Double]-> Double
dnrm2 = sqrt . sum . map (\i -> i * i)

dznrm2 ::  [Double] -> Double
dznrm2 []   = 0
dznrm2 x = let (ssq,scale) = foldr (\e (s,c) -> if c<(abs e)  
                                                  then (1+s*(c/ (abs e))*(c/ (abs e)), abs e) 
                                                  else (s+((abs e)/c)*((abs e)/c), c)) (1,0)  x
            in scale * (sqrt ssq) 


dasum :: [Double] -> Double
dasum []=0
dasum (x:xs)  = abs x+  dasum xs

idamax :: [Double] ->  Maybe Int
ida ::  [Double]->[Double]
ida [x]=[abs(x)]
ida (x:xs) =abs(x): ida xs
idamax []=Nothing
idamax a=elemIndex (maximum (ida a)) (ida a)


gam=4096.0
gamsq=16777216.0
rgamsq=0.000000059604645

f_2_4:: Double->Double->Double->Double->Double->Double->Bool
f_2_4 d1 x1 d2 y1 h12 h21| (abs (d1*x1*x1)> abs (d2*y1*y1)) && (1-h12*h21)>0 = True
                         | otherwise                                         = False

f_2_else_5_if:: Double->Double->Double->Double->Bool
f_2_else_5_if d1 x1 d2 y1 | (abs (d1*x1*x1)<= abs (d2*y1*y1)) && (d2*y1*y1)<0 = True
                          | otherwise                                         = False

f_2_else_5_else:: Double->Double->Double->Double->Bool
f_2_else_5_else d1 x1 d2 y1 | (abs (d1*x1*x1)<= abs (d2*y1*y1)) && (d2*y1*y1)>=0 = True
                            | otherwise                                          = False

f_6_8_if:: Double-> Bool
f_6_8_if d1 | d1 /=0 && ((d1<=rgamsq) || (d1>=gamsq)) && (d1<rgamsq) = True
            | otherwise                                              = False

f_6_8_else:: Double-> Bool
f_6_8_else d1 | d1 /=0 && ((d1<=rgamsq) || (d1>=gamsq)) && (d1>=rgamsq) = True
              | otherwise                                              = False

f_6_7_if:: Double-> Double-> Bool
f_6_7_if d1 flag | d1 /=0 && ((d1<=rgamsq) || (d1>=gamsq)) && flag==0     = True
            | otherwise                                              = False

f_6_7_else:: Double->Double-> Bool
f_6_7_else d1 flag | d1 /=0 && ((d1<=rgamsq) || (d1>=gamsq)) && flag/=0 = True
                   | otherwise                                             = False

f_9_10_if:: Double-> Double-> Bool
f_9_10_if d2 flag | (d2 /=0) && ((abs d2 <= rgamsq) || (abs d2>=gamsq) )  && flag==0 = True
                  | otherwise                                                        = False

f_9_10_else:: Double->Double-> Bool
f_9_10_else d2 flag | (d2 /=0) && ((abs d2 <= rgamsq) || (abs d2>=gamsq) )  && flag/=0 = True
                    | otherwise                                                        = False

f_9_11_if:: Double-> Bool
f_9_11_if d2 | d2 /=0 && ((d2<=rgamsq) || (d2>=gamsq)) && (d2<rgamsq) = True
             | otherwise                                              = False

f_9_11_else:: Double-> Bool
f_9_11_else d2 | d2 /=0 && ((d2<=rgamsq) || (d2>=gamsq)) && (d2>=rgamsq) = True
               | otherwise                                              = False

drotmg_d1 :: Double-> Double -> Double-> Double ->Double->Double->Double->Double->Double->Double
drotmg_d1  d1 d2 x1 y1 flag h11 h12 h21 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21   = d1/(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1   = 0
                                               | f_2_else_5_else d1 x1 d2 y1  =  d2/(1+(h11*h22))
                                               | f_6_8_if d1                 =  d1*(gam*gam)
                                               | f_6_8_else d1               =  d1/(gam*gam) 
                                               | otherwise =0

drotmg_d2 :: Double-> Double -> Double-> Double ->Double->Double->Double->Double->Double->Double
drotmg_d2  d1 d2 x1 y1 flag h11 h12 h21 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21  = d2/(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1  = 0
                                               | f_2_else_5_else d1 x1 d2 y1   =  d1/(1+(h11*h22))
                                               | f_9_11_if d2 =  d2*(gam*gam)
                                               | f_9_11_else d2 =  d2/(gam*gam) 
                                               | otherwise =0
                                         
drotmg_x1 :: Double-> Double -> Double-> Double ->Double->Double->Double->Double->Double->Double
drotmg_x1  d1 d2 x1 y1 flag h11 h12 h21 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21 = x1*(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1  = 0
                                               | f_2_else_5_else d1 x1 d2 y1 =  y1*(1+(h11*h22))
                                               | f_6_8_if d1 =  x1/gam
                                               | f_6_8_else d1 =  x1*gam 
                                               | otherwise = 0

drotmg_flag :: Double-> Double -> Double-> Double ->Double->Double->Double->Double->Double->Double
drotmg_flag  d1 d2 x1 y1 flag h11 h12 h21 h22  | d1<0           = negate 1--1
                                               | d2*y1==0      =negate 2--3
                                               | f_2_4 d1 x1 d2 y1 h12 h21 = 0--2-4
                                               | f_2_else_5_if d1 x1 d2 y1  = negate 1-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1   =  1-- 2-else-5-else
                                               | f_6_7_if d1 flag = negate 1-- 6-7-if
                                               | f_9_10_if d2 flag = negate 1 ----9-10-if
                                               | otherwise = negate 1


drotmg_h11 :: Double-> Double -> Double-> Double ->Double->Double->Double->Double->Double->Double
drotmg_h11  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1  =  (d1*x1)/(d2*y1)-- 2-else-5-else
                                               | f_6_7_if d1 flag =  1-- 6-7-if
                                               | f_6_8_if d1 =  h11/gam-- 6-8-if
                                               | f_6_8_else d1 =  h11*gam -- 6-8-else
                                               | f_9_10_if d2 flag = 1 ----9-10-if
                                               | otherwise = 0


drotmg_h12 :: Double-> Double -> Double-> Double ->Double->Double->Double->Double->Double->Double
drotmg_h12  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | abs (d1*x1*x1)> abs (d2*y1*y1)  = (d2*y1)/(d1*x1)-- 2-if
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_6_7_else d1 flag =  1-- 6-7-else
                                               | f_6_8_if d1 =  h12/gam-- 6-8-if
                                               | f_6_8_else d1 =  h12*gam -- 6-8-else
                                               | f_9_10_else d2 flag = 1 ----9-10-else
                                               | otherwise = 0


drotmg_h21 :: Double-> Double -> Double-> Double ->Double->Double->Double->Double->Double->Double
drotmg_h21  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | abs (d1*x1*x1)> abs (d2*y1*y1)  = negate (y1/x1)-- 2-if
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_6_7_else d1 flag = negate 1-- 6-7-else
                                               | f_9_10_else d2 flag = negate 1 ----9-10-else
                                               | f_9_11_if d2 =  h21/gam-- 9-11-if
                                               | f_9_11_else d2 =  h21*gam -- 9-11-else
                                               | otherwise = 0
                                       
drotmg_h22 :: Double-> Double -> Double-> Double ->Double->Double->Double->Double->Double->Double
drotmg_h22  d1 d2 x1 y1 flag h11 h12 h21 h22   | d1<0           = 0--1
                                               | f_2_else_5_if d1 x1 d2 y1 = 0-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1  =  x1/y1-- 2-else-5-else
                                               | f_6_7_if d1 flag =  1-- 6-7-if
                                               | f_9_10_if d2 flag = 1 ----9-10-if
                                               | f_9_11_if d2 =  h22/gam-- 9-11-if
                                               | f_9_11_else d2 =  h22*gam -- 9-11-else
                                               | otherwise = 0

drotmg :: Double-> Double -> Double-> Double ->[Double]->(Double,Double,Double,[Double])
drotmg d1 d2 x1 y1 dparam= 
    let flag=dparam !! 0
        h11=dparam !! 1
        h21= dparam !! 2
        h12=dparam !! 3
        h22=dparam !! 4 
    in (drotmg_d1 d1 d2 x1 y1 flag h11 h21 h12 h22,drotmg_d2 d1 d2 x1 y1 flag h11 h21 h12 h22,drotmg_x1 d1 d2 x1 y1 flag h11 h21 h12 h22, [drotmg_flag d1 d2 x1 y1 flag h11 h21 h12 h22,drotmg_h11 d1 d2 x1 y1 flag h11 h21 h12 h22,drotmg_h21 d1 d2 x1 y1 flag h11 h21 h12 h22,drotmg_h12 d1 d2 x1 y1 flag h11 h21 h12 h22,drotmg_h22 d1 d2 x1 y1 flag h11 h21 h12 h22])
               

main = do
        let x= [1..5]
        let y=[1..5]
        let param=take 5 (repeat 2)
        let c=1.0
        let s=2.0
        let alpha=3.0
        let a=5.0
        let m=8.0
        let n= 9.0
        let d1=1000000000.0
        let d2=1.0
        let x1=2.0
        let x2=4.0
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
  
--Mamtajs-MacBook-Pro:~ mamtajakter$ cd documents/haskell
--Mamtajs-MacBook-Pro:haskell mamtajakter$ ghc --make hblas.hs
--Mamtajs-MacBook-Pro:haskell mamtajakter$ ./hblas

--Mamtajs-MacBook-Pro:haskell mamtajakter$ ghc -fllvm -keep-llvm-files -fforce-recomp hblas.hs
