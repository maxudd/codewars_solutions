import Data.List

snailHelp :: [[Int]] -> Int -> Int -> [([Int], Int)] 
-- snailHelp [] _ _ = []
snailHelp [x] i n = [(reverse x, n)]
snailHelp sq@(x:xs) i n = case i of
    1 -> (x, 1) : snailHelp xs (i + 1) n
    _ -> ([head x], 2*n - i) : ([last x], i) : snailHelp xs (i + 1) n

snail :: [[Int]] -> [Int]
snail [] = []
snail [[]] = []
snail [[x]] = [x] 
snail square = part1 ++ part2
    where part1 = snailHelp1 square size
          part2 = snail (cropSquare square)
          size = length square

snailHelp1 :: [[Int]] -> Int -> [Int]
snailHelp1 square n = concatMap fst $ sortOn snd $ snailHelp square 1 n

cropSquare :: [[Int]] -> [[Int]]
cropSquare (x:xs) = cropSquareHelp xs 

cropSquareHelp :: [[Int]] -> [[Int]]
cropSquareHelp [x] = []
cropSquareHelp [x, y] = [cropString x]
cropSquareHelp (x1:xs) = cropString x1 : cropSquareHelp xs

cropString :: [Int] -> [Int]
cropString (x:xs) = init xs

array2 = [[1,2],[4,3]] :: [[Int]]
array3 = [[1,2,3],[8,9,4],[7,6,5]] :: [[Int]]
array3_1 = [[1,2,3],[4,5,6],[7,8,9]] :: [[Int]]
array5 = [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20],[21,22,23,24,25]] :: [[Int]]
array3_2 = [[7,8,9],[12,13,14],[17,18,19]]:: [[Int]]
array6 = [[1,2,3,4,5,0],[6,7,8,9,10,0],[11,12,13,14,15,0],[16,17,18,19,20,0],[21,22,23,24,25,0],[0,0,0,0,0,0]] :: [[Int]]