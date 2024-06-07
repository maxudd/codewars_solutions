-- UNFINISHED


import Data.List ( sort, zipWith3, sortOn )
import Data.Char
import GHC.Base (VecElem(Int16ElemRep))


-- data Sign = Pos String | Neg String deriving (Show)
-- data Monomial = Pos String String | Neg String String deriving Show
type Monomial = (String, String)

trans :: String -> String
trans str@('-':_) = str
trans str = '+':str

isPlusMinus :: Char -> Bool
isPlusMinus '+' = True
isPlusMinus '-' = True
isPlusMinus _ = False

-- isChangeState :: Char -> Char -> Bool
-- isChangeState x y = isDigit x && isLetter y ||  || isPlusMinus x && isLetter y

fun :: String -> String -> [String] -> [String] -> [Monomial]
-- fun [x] buffer nakop1 nakop2 = zip (map reverse nakop1) (map sort nakop2)
-- fun [x,y] buffer nakop1 nakop2 = 
-- fun [x,y] buffer nakop1 nakop2 = nakop2
fun [x] buffer nakop1 nakop2 | isDigit x = zip (map reverse ((x:buffer) : nakop1)) (map sort ("":nakop2))
                             | isLetter x = zip (map reverse nakop1) (map sort ((x:buffer):nakop2))
fun (x1:x2:xs) buffer nakop1 nakop2 | isDigit x1 && isDigit x2 || isLetter x1 && isLetter x2 || x1 == '-' && isDigit x2 = fun (x2:xs) (x1:buffer) nakop1 nakop2
                                    | x1 == '+' && isDigit x2 = fun (x2:xs) buffer nakop1 nakop2
                                    -- | isDigit x1 && isLetter x2 || isPlusMinus x1 && isLetter x2 = fun (x2:xs) [] ((x1:buffer) : nakop1) nakop2
                                    | isDigit x1 && isLetter x2  = fun (x2:xs) [] ((x1:buffer) : nakop1) nakop2
                                    | x1 == '-' && isLetter x2 = fun (x2:xs) [] ("1-":nakop1) nakop2
                                    | x1 == '+' && isLetter x2 = fun (x2:xs) [] ("1":nakop1) nakop2
                                    | isLetter x1 && isPlusMinus x2 = fun (x2:xs) [] nakop1 ((x1:buffer) : nakop2)
                                    | isDigit x1 = fun (x2:xs) [] ((x1:buffer) : nakop1) ("" : nakop2)


-- fun2 :: String -> String -> [String] -> [String] -> [Monomial]
-- fun2 (x1:x2:xs)

-- fun2 (a,b) (c,d) | b == d = (show $ ra+rc,b)
--                  | 

remdumps [] = []
remdumps xs = foldr a [] xs
                    --  r1 = read x1 :: Int
                    --  r2 = read x2 :: Int

(x1,y1) `a` ((x2,y2):xs) | y1 == y2 = (show $ r1 + r2, y1) : xs
                                        where r1 = read x1 :: Int
                                              r2 = read x2 :: Int
(x,y) `a` xs = (x,y):xs


fun2 ("0",_) = []
fun2 ("1", b) = b
fun2 (a,b) = a ++ b