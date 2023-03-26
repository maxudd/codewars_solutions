createPhoneNumber :: [Int] -> String
createPhoneNumber xs = "(" ++ first ++ ") " ++ second ++ "-" ++ third
                  where (first, other1) = splitAt 3 $ concatMap show xs
                        (second, third) = splitAt 3 other1