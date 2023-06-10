possibilities :: String -> [String]
possibilities "" = [""]
possibilities ('?':xs) = possibilities ('1' : xs) ++ possibilities ('0' : xs)
possibilities str@(x:xs) | '?' `elem` xs = map (x:) $ possibilities xs 
                         | otherwise     = [str] 