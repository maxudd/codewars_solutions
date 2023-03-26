formatWords :: [String] -> String
formatWords xs = formatWords1 $ filter (not.null) xs

formatWords1 :: [String] -> String 
formatWords1 [] = ""
formatWords1 [x] = x
formatWords1 (x1:xs2@(x2:xs)) | null x1 = formatWords xs2
                              | null xs = x1 ++ " and " ++ x2
                              | otherwise = x1 ++ ", " ++ formatWords xs2