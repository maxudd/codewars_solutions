crash :: [Int] -> [Int] -> [Int]
crash [] _ = []
crash (x:xs) (y:ys) | x > y     = x + y : crash xs ys
                    | otherwise = y : crash xs ys


crashingWeights :: [[Int]] -> [Int]
crashingWeights = foldl1 crash