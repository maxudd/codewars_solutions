import Data.List

humanReadable :: Int -> String
humanReadable x = intercalate ":" $ map myshow [hh, mm2,ss2]
                  where (ss1,ss2) = x `divMod` 60
                        (hh,mm2) = ss1 `divMod` 60

myshow x | x < 10    = '0':show x
         | otherwise = show x