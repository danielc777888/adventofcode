

main :: IO()
main = interact solve

type Food = ([Ingredient], [Allergen])
type Ingredient = String
type Allergen = String

solve :: String -> String
solve = show . counts . allergenFree .  map food . lines
--solve = show . map food . lines

food :: String -> Food
food xs = (is, as')
  where (is, as) = break (=="(contains") (words xs)
        as' = (drop 1 (init as)) ++ [init (last as)]

allergenFree :: [Food] -> ([Food], [Ingredient])
--allergenFree xs = (xs, ["sbzzf","kfcds","trh","nhms"])
allergenFree xs = (xs, ["sbzzf","kfcds","trh","nhms"])

counts :: ([Food], [Ingredient]) -> Int
counts ([], _) = 0
counts (x:xs, ys) = counts' (fst x, ys) + counts (xs, ys) 

counts' :: ([Ingredient], [Ingredient]) -> Int
counts' ([], _) = 0
counts' (x:xs, ys) = if x `elem` ys then 1 + counts' (xs, ys) else counts' (xs, ys)
