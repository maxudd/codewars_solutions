formatDuration :: (Integral i, Show i) => i -> String
formatDuration 0 = "now"
formatDuration n = formatWords $ zipWith format [yy, dd2, hh2, mm2, ss2] ydhms 
                  where (ss1,ss2) = n `divMod` 60
                        (hh,mm2) = ss1 `divMod` 60
                        (dd,hh2) = hh `divMod` 24
                        (yy,dd2) = dd `divMod` 365

ydhms :: [(String, String)]
ydhms = [(" year"," years"), (" day", " days"), (" hour", " hours"), (" minute", " minutes"), (" second", " seconds")]

format :: (Integral i, Show i) => i -> (String, String) -> String 
format 0 _ = ""
format 1 (t,_) = "1" ++ t
format x (_,t1) = show x ++ t1

--additional useful functions taken from another kata
formatWords :: [String] -> String
formatWords xs = formatWords1 $ filter (not.null) xs

formatWords1 :: [String] -> String
formatWords1 [] = ""
formatWords1 [x] = x
formatWords1 (x1:xs2@(x2:xs)) | null x1 = formatWords xs2
                              | null xs = x1 ++ " and " ++ x2
                              | otherwise = x1 ++ ", " ++ formatWords xs2