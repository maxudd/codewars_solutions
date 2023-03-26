import Data.Word  (Word32)
import Data.List (intercalate)

type IPString = String

word32ToIP :: Word32 -> IPString
word32ToIP word32 = intercalate "." $ map show $ reverse $ map (\l -> sum $ zipWith (\x y -> x * 2^y) l [0..]) $ groupEight $ take 32 $ toBinary word32 ++ [0,0..]

toBinary :: Word32 -> [Word32]
toBinary 0 = []
toBinary 1 = [1]
toBinary n = mod n 2 : toBinary (div n 2)

groupEight :: [Word32] -> [[Word32]]
groupEight [] = []
groupEight ls = take 8 ls : groupEight (drop 8 ls)