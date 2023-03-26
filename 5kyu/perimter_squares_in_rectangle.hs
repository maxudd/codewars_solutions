perimeter :: Integer -> Integer
perimeter n = 4 * sum (take (fromEnum n + 2) fibs)
              where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)