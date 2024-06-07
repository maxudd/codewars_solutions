numbers = [1..9]

findAdjacent :: Int -> [Int]
findAdjacent 0 = [8]
findAdjacent 8 = [5, 7, 9, 0]
findAdjacent n = filter (`elem` numbers) [n - 3, n - 1, n + 1, n + 3]

