countInversions :: Ord a => [a] -> Int
countInversions [] = 0
countInversions (x:xs) = length (filter (< x) xs) + countInversions xs