import Data.List (transpose)

landPerimeter :: [String] -> String
landPerimeter arr = "Total land perimeter: " ++ show (rows + columns)
                    where rows    = sum $ map truePerimeter arr
                          columns = sum $ map truePerimeter $ transpose arr

truePerimeter :: String -> Int
truePerimeter str@(x:_) | x == 'X'  = perimetr str - 2*xInXs
                        | otherwise = perimetr str - 2*xInStr
                           where xInStr = length $ filter (=='X') str
                                 xInXs  = xInStr - 1

perimetr :: String -> Int
perimetr ['X']            = 2
perimetr ('X':xs@('O':_)) = 2 + perimetr xs
perimetr ('X':xs@('X':_)) = 2 + perimetr xs
perimetr ('O':xs@('X':_)) = 2 + perimetr xs
perimetr ('O':xs@('O':_)) = perimetr xs
perimetr _                = 0