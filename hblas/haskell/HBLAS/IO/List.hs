module HBLAS.IO.List where

listFromFile :: (Floating n, Read n) => FilePath -> IO [n]
listFromFile f = fmap read . lines <$> readFile f

--matrixFromFile :: (Floating n, Read n) => FilePath -> IO [[n]]
--matrixFromFile f = fmap (read . (++"]") . ('[':)) . lines <$> readFile f
