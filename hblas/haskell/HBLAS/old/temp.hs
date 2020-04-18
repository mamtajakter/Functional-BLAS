

module HBLAS.Temp where

{-


pattern Empty :: (Num n) => V.Vector n
pattern Empty <- (V.null -> True) where Empty = V.empty

uncons ::  (Num n) => V.Vector n -> Maybe (n, V.Vector n)
uncons Empty = Nothing
uncons v     = Just (V.unsafeHead v, V.unsafeTail v)

pattern (:<|)  ::  (Num n) => n -> V.Vector n -> V.Vector n
pattern x :<| xs <- (uncons -> Just (x, xs))

pattern (:|>) ::  (Num n) => V.Vector n -> n -> V.Vector n
pattern xs :|> x <- (uncons -> Just (x, xs))


trsvg :: ( Num n)=> V.Vector n-> Int-> n-> V.Vector n
trsvg (x :<| xs) i z
       | i>0       = pure x <> (trsvg xs (i-1) z)
       | otherwise = pure z <> xs
-}

{-

--Left

--Done matched


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
f_6_8_if d1 _
              |  (d1 >0) && (d1<rgamsq)= True
              |  otherwise             = False

f_6_8_else :: (Floating n, Ord n) => n -> Bool -> Bool
f_6_8_else d1 _
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

rotmg_d1 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
rotmg_d1  d1 d2 x1 y1 flag h11 h21 h12 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21   = d1/(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1   = 0
                                               | f_2_else_5_else d1 x1 d2 y1  =  d2/(1+(h11*h22))
                                               | f_6_8_if d1 flag               =  d1*(gam*gam)
                                               | f_6_8_else d1 flag           =  d1/(gam*gam)
                                               | otherwise =0

rotmg_d2 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
rotmg_d2  d1 d2 x1 y1 _ h11 h21 h12 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21  = d2/(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1  = 0
                                               | f_2_else_5_else d1 x1 d2 y1   =  d1/(1+(h11*h22))
                                               | f_9_11_if d1 d2 =  d2*(gam*gam)
                                               | f_9_11_else d1 d2 =  d2/(gam*gam)
                                               | otherwise =0

rotmg_x1 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
rotmg_x1  d1 d2 x1 y1 flag h11 h21 h12 h22    | d1<0           = 0--1
                                               | f_2_4 d1 x1 d2 y1 h12 h21 = x1*(1-(h12*h21))
                                               | f_2_else_5_if d1 x1 d2 y1  = 0
                                               | f_2_else_5_else d1 x1 d2 y1 =  y1*(1+(h11*h22))
                                               | f_6_8_if d1 flag=  x1/gam
                                               | f_6_8_else d1 flag =  x1*gam
                                               | otherwise = 0

rotmg_flag :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
rotmg_flag  d1 d2 x1 y1 flag _ h21 h12 _  | d1<0           = negate 1--1
                                               | d2*y1==0      =negate 2--3
                                               | f_2_4 d1 x1 d2 y1 h12 h21 = 0--2-4
                                               | f_2_else_5_if d1 x1 d2 y1  = negate 1-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1   =  1-- 2-else-5-else
                                               | f_6_7_if d1 flag = negate 1-- 6-7-if
                                               | f_9_10_if d1 d2 flag = negate 1 ----9-10-if
                                               | otherwise = negate 1


rotmg_h11 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
rotmg_h11  d1 d2 x1 y1 flag h11 _ _ _   | d1<0           = 0--1
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1  =  (d1*x1)/(d2*y1)-- 2-else-5-else
                                               | f_6_7_if d1 flag =  1-- 6-7-if
                                               | f_6_8_if d1 flag =  h11/gam-- 6-8-if
                                               | f_6_8_else d1 flag =  h11*gam -- 6-8-else
                                               | f_9_10_if d1 d2 flag = 1 ----9-10-if
                                               | otherwise = 0


rotmg_h12 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
rotmg_h12  d1 d2 x1 y1 flag _ h12 _ _   | d1<0           = 0--1
                                               | abs (d1*x1*x1)> abs (d2*y1*y1)  = (d2*y1)/(d1*x1)-- 2-if
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_6_7_else d1 flag =  1-- 6-7-else
                                               | f_6_8_if d1 flag=  h12/gam-- 6-8-if
                                               | f_6_8_else d1 flag =  h12*gam -- 6-8-else
                                               | f_9_10_else d1 d2 flag = 1 ----9-10-else
                                               | otherwise = 0


rotmg_h21 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
rotmg_h21  d1 d2 x1 y1 flag _ _ h21 _   | d1<0           = 0--1
                                               | abs (d1*x1*x1)> abs (d2*y1*y1)  = negate (y1/x1)-- 2-if
                                               | f_2_else_5_if d1 x1 d2 y1  = 0-- 2-else-5-if
                                               | f_6_7_else d1 flag = negate 1-- 6-7-else
                                               | f_9_10_else d1 d2 flag = negate 1 ----9-10-else
                                               | f_9_11_if d1 d2 =  h21/gam-- 9-11-if
                                               | f_9_11_else d1 d2 =  h21*gam -- 9-11-else
                                               | otherwise = 0

rotmg_h22 :: (Floating n, Ord n) => n -> n -> n -> n -> Bool -> n -> n -> n -> n -> n
rotmg_h22  d1 d2 x1 y1 flag _ _ _ h22   | d1<0           = 0--1
                                               | f_2_else_5_if d1 x1 d2 y1 = 0-- 2-else-5-if
                                               | f_2_else_5_else d1 x1 d2 y1  =  x1/y1-- 2-else-5-else
                                               | f_6_7_if d1 flag =  1-- 6-7-if
                                               | f_9_10_if d1 d2 flag = 1 ----9-10-if
                                               | f_9_11_if d1 d2 =  h22/gam-- 9-11-if
                                               | f_9_11_else d1 d2 =  h22*gam -- 9-11-else
                                               | otherwise = 0

rotmg :: (Applicative f, Indexable f, Monoid (f n), Floating n, Ord n)
       => n -> n -> n -> n -> f n -> (n,n,n,f n)
rotmg d1 d2 x1 y1 dparam=
    let flag= if (dparam !! 0) >0 then True else False
        h11=dparam !! 1
        h21= dparam !! 2
        h12=dparam !! 3
        h22=dparam !! 4
    in (rotmg_d1 d1 d2 x1 y1 flag h11 h21 h12 h22,rotmg_d2 d1 d2 x1 y1 flag h11 h21 h12 h22,rotmg_x1 d1 d2 x1 y1 flag h11 h21 h12 h22, pure (rotmg_flag d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (rotmg_h11 d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (rotmg_h12 d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (rotmg_h21 d1 d2 x1 y1 flag h11 h21 h12 h22) <> pure (rotmg_h22 d1 d2 x1 y1 flag h11 h21 h12 h22))
-}
import HBLAS.Class
import HBLAS.Level1
import Prelude hiding (zipWith,(!!), foldl')
import Data.Monoid
-- import Data.Foldable (foldl')
-- import Data.List hiding (zipWith)

-- gemv :: (HBLAS m, Num n) => m (m n) -> m n -> m n -> n -> n-> m n
-- gemv matA vecX vecY alpha beta= let x1= fmap (dot vecX) matA
--                                     y1= scal beta vecY
-- 	                            in axpy alpha x1 y1
trmv :: (HBLAS m, Num n) => m (m n) -> m n   -> m n
trmv matA vecX n
               |
	           matA.length ->
	           _           -> trmvRow matA vecX n <> trmv matA vecX (n-1)
	           where trmvRow matA vecX n = dot (matA !! i) (vecX !! i)


gemv1 :: (HBLAS m, Num n) => m (m n) -> m n -> m n
gemv1 mat vec = fmap (dot vec) mat

-- numRows :: [[Int]] -> Int
-- numRows = length
-- takes a matrix and returns number of rows

numRows :: (HBLAS m, Num n) => m (m n) -> Int
numRows =  length

-- numColumns :: [[Int]] -> Int
-- numColumns = length . head

-- take n, applied to a list xs, returns the prefix of xs of length n, or xs itself if n > length xs:
-- take 5 "Hello World!" == "Hello"
-- take 3 [1,2,3,4,5] == [1,2,3]
-- take 3 [1,2] == [1,2]

take' :: (HBLAS m, Monoid (m n), Num n)=> Int -> m n -> m n
take' 0 _ = mempty
take' n' x = pure (x !! n') <> take' (n'-1) x

-- drop n xs returns the suffix of xs after the first n elements, or [] if n > length xs:

-- drop 6 "Hello World!" == "World!"
-- drop 3 [1,2,3,4,5] == [4,5]
-- drop 3 [1,2] == []

drop' :: (HBLAS m, Monoid (m n), Num n)=> Int -> m n -> m n
drop' 0 _ = mempty
drop' n' x = pure (x !! n') <> drop' (n'+1) x

-- cut :: [a] -> Int -> [a]
-- cut [ ] n = [ ]
-- cut xs n
--       | n < 1 || n > (length xs) = xs
--       | otherwise = (take (n-1) xs) ++ drop n xs


-- cut [1,2,3,4,5] 3 returns [1,2]++[4,5]= [1,2,4,5],
-- cut funtion takes a vector and an index,
-- then returns a list removing that element of the index

cut :: (HBLAS m, Monoid (m n), Num n) => m n -> Int ->  m n
cut xs n
      | n < 1 || n > (length xs) = xs
      | otherwise = (take' (n-1) xs) <> drop' n xs

-- takes a matrix and returns number of columns
numColumns ::  (HBLAS m, Num n) => m (m n) -> Int
numColumns  mat =   length ( mat !! 0)


-- transpose:: [[a]]->[[a]]
-- transpose ([]:_) = []
-- transpose x = (map head x) : transpose (map tail x)

--transpose [[1,2,3],[3,4,5],[5,6,7]]=[[1,3,5],[2,4,6],[3,5,7]]

transpose' :: (HBLAS m, Monoid (m (m n)), Num (m n)) =>  m (m n) -> m (m n)
-- transpose' ([]:_) = []
transpose' x = pure (x !! 0) <> transpose' (drop' 1 x)

-- remove :: [[Int]] -> Int -> Int -> [[Int]]
-- remove m i j
--       | m == [ ] || i < 1 || i > numRows m || j < 1 || j > numColumns m
--                                                         = error "remove: (i,j) out of range"
--       | otherwise = transpose ( cut (transpose ( cut m i ) ) j )

-- remove [[1,2,3],[3,4,5],[5,6,7]] 2 3 = cut m i deletes [3,4,5] and then transpose [[1,2,3],[5,6,7]]=[[1,5],[2,6],[3,7]]
-- cut [[1,5],[2,6],[3,7]] j= [[1,5],[2,6]] and then transpose [[1,5],[2,6]]= [[1,2],[5,6]]
-- remove [[1,2,3],[3,4,5],[5,6,7]] 2 3 = [[1,2],[5,6]]

remove :: (HBLAS m, Monoid (m (m n)), Num (m n)) => m (m n) -> Int-> Int -> m (m n)
remove m i j = transpose' ( cut (transpose' ( cut m i ) ) j )

-- determinant :: [[Int]] -> Int
-- determinant [ ] = error "determinant: 0-by-0 matrix"
-- determinant [[n]] = n
-- determinant m = sum [ (-1)^ (j+1) * (head m)!!(j-1) * determinant (remove m 1 j) | j <- [1..(numColumns m) ] ]

-- determinant [[1,2,3],[3,4,5],[5,6,7]] = 1 (4*7-5*6) - 2 (3*7-5*5) + 3 (3*6 - 4*5)
--   (-1)^ 2 * [1,2,3]!!0 * determinant (remove [[1,2,3],[3,4,5],[5,6,7]] 1 1= [[4,5],[6,7]])
-- + (-1)^ 3 * [1,2,3]!!1 * determinant (remove [[1,2,3],[3,4,5],[5,6,7]] 1 2= [[3,5],[5,7]])
-- + (-1)^ 4 * [1,2,3]!!2 * determinant (remove [[1,2,3],[3,4,5],[5,6,7]] 1 3= [[3,4],[5,6]])

determinant' :: (HBLAS m, Monoid (m (m n)), Num (m n), Num n) => m (m n) -> n
determinant' m = sum [ (-1)^ (j+1) * ( m!!0 )!!(j-1) * determinant' (remove m 1 j) | j <- [1..(numColumns m) ] ]
-- determinant' _ 0= 0
-- determinant' m j=  sum (pure (((-1)^(j+1)) * ((m!!0)!!(j -1) * determinant' (remove m 1 j ) (j-1))))

-- cofactor :: [[Int]] -> Int -> Int -> Int
-- cofactor m i j = (-1)^ (i+j) * determinant (remove m i j)

--cofactor [[1,2,3],[3,4,5],[5,6,7]] 1 1 = 4*7-5*6= -2
cofactor :: (HBLAS m, Monoid (m (m n)), Num (m n), Num n) => m (m n) -> Int-> Int-> n
cofactor m i j = (-1)^ (i+j) * determinant' (remove m i j)

-- cofactorMatrix :: [[Int]] -> [[Int]]
-- cofactorMatrix m = [ [ (cofactor m i j) | j <- [1..n] ] | i <- [1..n] ]
--                    where
--                    n = length m

--cofactorRow [[1,2,3],[3,4,5],[5,6,7]] 1 = [-2, -1, -2]
cofactorRow :: (HBLAS m, Monoid (m n),Monoid (m (m n)), Num (m n), Num n) => m (m n) -> Int-> m n
cofactorRow m i= pure (cofactor m i i) <> (cofactorRow m i-1)

--cofactorMatrix [[1,2,3],[3,4,5],[5,6,7]] 3 = [[-2, -1, -2], [-4,8,-4],[-4,8,-4]]

cofactorMatrix :: (HBLAS m, Monoid (m n),Monoid (m (m n)), Num (m n),(Num (m (m n))), Num n) => m (m n) -> Int-> m (m n)
cofactorMatrix m j =   pure (cofactorRow m j ) <> (cofactorMatrix m j-1)

-- inverse :: [[Int]] -> [[Int]]
-- inverse m = transpose [ [ quot x (determinant m) | x <- row ] | row <- (cofactorMatrix m) ]

-- inverseRow :: (HBLAS m, Monoid (m n),Monoid (m (m n)), Num (m n), Num n, (Num (m (m n))), Num n) => m (m n) -> Int-> m n
-- inverseRow m i = pure ((m !! i) / (determinant' m)) <> (inverseRow m i-1)

-- inverse :: (HBLAS m, Monoid (m n),Monoid (m (m n)), Num (m n),(Num (m (m n))), Integral n) => m (m n) ->Int-> m (m n)
-- inverse m j = transpose' (pure (inverseRow m j) <> pure (inverse m j-1))

-- trsv :: (HBLAS m, Monoid (m n),Monoid (m (m n)), Num (m n),(Num (m (m n))), Integral n) => m (m n) ->m n-> m (m n)
-- trsv m v = pure (sum (zipWith (*) (m !! 0) v)) <> trsv

-- trsv :: [[Int]] ->[Int]-> [[Int]]
-- trsv mat vec = [[ sum $ zipWith (*) ar vec ] | ar <- (inverse mat) ]


-----------------

module HBLAS.Level2 where

import HBLAS.Class
import HBLAS.Level1
import Prelude hiding (zipWith,(!!), foldl')
import Data.Monoid

-- import Data.Foldable (foldl')
-- import Data.List hiding (zipWith)

-- gemv :: (HBLAS m, Num n) => m (m n) -> m n -> m n
-- gemv mat vec = fmap (dot vec) mat

{-
printf("Upper");
for (i=0;i<3;i++){
    for (j=i; j<3; j++){
        printf("%d,", mm[i][j] * vv[j][0]);
        cc[i][0]+= mm[i][j] * vv[j][0];
      }
    printf("%d\n", cc[i][0]);
    }
-}

reverse' :: (HBLAS m, Monoid (m n), Num n)=>  m n-> Int -> m n
reverse'  _ 0 = mempty
reverse' n x = pure (x !! n') <> reverse' (n'-1) x


take' :: (HBLAS m, Monoid (m n), Num n)=> Int -> m n -> m n
take' 0 _ = mempty
take' n' x = pure (x !! n') <> take' (n'-1) x

transpose' :: (HBLAS m, Monoid (m n),Monoid (m(m n)), Num n)=>  m n ->Int-> m(m n)
transpose' mempty _ = pure mempty
transpose' x n' = pure (pure (x !! n')) <> transpose' x (n'-1)

gemv :: (HBLAS m, Num n) => m (m n) -> m n -> m n -> n -> n -> m n
gemv matA vecX vecY alpha beta=
    let x1 = fmap (dot vecX) matA
        y1= scal beta vecY
    in  axpy alpha x1 y1

trmv :: (HBLAS m, Num n, Monoid (m n)) => m (m n) -> m n -> Char -> Int-> m n
trmv mat vec uplo n
          | uplo=='u' = upper mat vec n
          | otherwise = lower mat vec n

upper :: (HBLAS m, Num n, Monoid (m n)) => m (m n) -> m n -> Int-> m n
upper mat vec 1= pure (dot vec (mat !! 0))
upper mat vec n= pure (dot vec (mat !! (n-1) ) ) <> upper mat vec (n-2)

lower :: (HBLAS m, Num n, Monoid (m n)) => m (m n) -> m n -> Int-> m n
lower mat vec 1= pure (dot vec (mat !! 0))
lower mat vec n= pure (dot vec (mat !! (n-1) ) ) <> lower mat vec (n-2)

-- transpose               :: (HBLAS m, Num n) => m (m n) -> m (m n)
-- transpose []             = []
-- transpose ([]   : xss)   = transpose xss
-- transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

{-
ger :: (HBLAS m, Num n) => m (m n) -> m n -> m n -> n  -> m n
ger matA vecX vecY alpha=
    let x1 = fmap (dot vecX) matA
        y1= scal alpha vecY
    in  axpy alpha x1 y1
    in fmap (scal vecX) matA
-}
