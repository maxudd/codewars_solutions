removeIthElem :: Int -> [a] -> [a]
removeIthElem 0 (x:xs) = xs
removeIthElem i xs = a ++ tail b
             where (a, b) = splitAt i xs

det :: [[Int]] -> Int
det [[x]] = x
det (xs:xss) = sum [(-1)^(2+i) * xs !! i * det [removeIthElem i x | x <- xss] | i <- [0..(length xs - 1)]]
