import Data.Char (toUpper)


generateHashtag :: String -> Maybe String
generateHashtag ""                   = Nothing
generateHashtag xs | length gH > 140 = Nothing
                   | null gH         = Nothing
                   | otherwise       = Just ('#':gH)
                    where gH = concatMap (\(x:xs) -> toUpper x : xs) $ words xs