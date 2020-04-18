

numRows :: [[a]] -> Int
numRows = length


numColumns :: [[a]] -> Int
numColumns = length . head

cut :: [a] -> Int -> [a]
cut [ ] n = [ ]
cut xs n
      | n < 1 || n > (length xs) = xs
      | otherwise = (take (n-1) xs) ++ drop n xs

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

remove :: [[a]]  -> Int -> Int -> [[a]] 
remove m i j
           | m == [ ] || i < 1 || i > numRows m || j < 1 || j > numColumns m = error "remove: (i,j) out of range"
           | otherwise = transpose ( cut (transpose ( cut m i ) ) j )

determinant :: [[a]]  -> Int
determinant [ ] = error "determinant: 0-by-0 matrix"
determinant [[n]] = n
determinant m = sum [ (-1)^ (j+1) * (head m)!!(j-1) * determinant (remove m 1 j) | j <- [1..(numColumns m) ] ]

cofactor :: [[a]]  -> Int -> Int -> Int
cofactor m i j = (-1)^ (i+j) * determinant (remove m i j)


cofactorMatrix :: [[a]]  -> [[a]] 
cofactorMatrix m = [ [ (cofactor m i j) | j <- [1..n] ] | i <- [1..n] ]
                   where
                   n = length m


inverse :: [[a]]  -> [[a]] 
inverse m = transpose [ [ quot x (determinant m) | x <- row ] | row <- (cofactorMatrix m) ]

