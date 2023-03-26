moveZeros :: [Int] -> [Int]
moveZeros xs = x ++ y
              where x = filter (/= 0) xs
                    y = filter (==0) xs