import Data.List ( intercalate )

solution :: [Integer] -> String
solution list = intercalate "," (reverse $ solution2 list [] [])

update :: [Integer] -> [String] -> [String]
update [x,y] nakop = show x : show y : nakop
update (x:xs) nakop = (show (last xs) ++ "-" ++ show x) : nakop 

solution2 :: [Integer] -> [Integer] -> [String] -> [String]
solution2 [x] [] nakop = show x : nakop
solution2 [x] nakop1 nakop2 = update (x:nakop1) nakop2
solution2 (x1:x2:xs) [] nakop = case x2 - x1 of
    1 -> solution2 (x2:xs) [x1] nakop
    _ -> solution2 (x2:xs) [] (show x1 : nakop)
solution2 (x1:x2:xs) nakop1 nakop2 = case x2 - x1 of
    1 -> solution2 (x2:xs) (x1 : nakop1) nakop2
    _ -> solution2 (x2:xs) [] (update (x1:nakop1) nakop2)