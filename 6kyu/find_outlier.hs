import Data.List (find)

findOutlier :: [Int] -> Int 
findOutlier xs = case findSus xs of
        Just x -> x

findSus :: [Int] -> Maybe Int
findSus (x1:x2:list@(x3:xs)) | even x1 && even x2 = find odd list
                             | odd x1 && odd x2 = find even list
                             | odd x1 == odd x3 = Just x2
                             | otherwise = Just x1