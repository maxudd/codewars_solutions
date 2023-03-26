type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage = minimum $ listAmounts recipe storage

listAmounts :: Recipe -> Storage -> [Int]
listAmounts [] _ = []
listAmounts (r:recipes) storage = howMuch r storage : listAmounts recipes storage

howMuch :: (String, Int) -> [(String, Int)] -> Int
howMuch (what, hm) storage = case lookup what storage of
      Just x -> x `div` hm
      Nothing -> 0