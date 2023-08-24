data User = User {
    rank :: Int,
    progress :: Int
} deriving Show

ranks = [-8,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8]

newUser :: User
newUser = User (-8) 0

(/-/) :: Int -> Int -> Int
a /-/ u | a > 0 && u < 0 = a - u - 1
        | a < 0 && u > 0 = a - u - 2
        | otherwise = a - u

incProgress :: Int -> User -> User
incProgress rank_a u@(User rank_u prog) | rank_a `notElem` ranks || rank_u `notElem` ranks = error "Invalid rank"
                                        | rank_u == 8 = u
                                        | rank_a == rank_u = upgrade $ User rank_u (prog + 3)
                                        | rank_u /-/ rank_a == 1 = upgrade $ User rank_u (prog + 1)
                                        | d > 0 = upgrade $ User rank_u (prog + 10 * d * d)
                                        | otherwise = u
                                            where d = rank_a /-/ rank_u

upgrade :: User -> User
upgrade (User 7 p) | p >= 100 = User 8 0
upgrade (User (-1) p) | p >= 100 = upgrade $ User 1 (p - 100)
upgrade u@(User r p)  | p >= 100 = upgrade $ User (r + 1) (p - 100)
                      | otherwise = u
                      