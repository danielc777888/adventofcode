-- 1766400

import Data.List

data Ingredient = Ingredient
  { name       :: String,
    capacity   :: Int,
    durability :: Int,
    flavour    :: Int,
    texture    :: Int,
    calories   :: Int
  }
  deriving (Show)

type Cookie = [Int]

main = interact solve

solve = show . maximum . scores score' . cookies . map ingredient . lines

ingredient :: String -> Ingredient
ingredient s = case words s of
  [n, _, c, _, d, _, f, _, t, _, cl] -> Ingredient n (parse c) (parse d) (parse f) (parse t) (read cl)
  where
    parse = read . init

cookies :: [Ingredient] -> ([Ingredient], [Cookie])
cookies xs = (xs, yss)
  where
    yss = concatMap permutations $ [[w, x, y, z] | w <- [1 .. 100], x <- [1 .. 100], y <- [1 .. 100], z <- [1 .. 100], w + x + y + z == 100]

scores :: ([Ingredient] -> Cookie -> Int) -> ([Ingredient], [Cookie]) -> [Int]
scores sc (xs, yss) = map (sc xs) yss

score :: [Ingredient] -> Cookie -> Int
score xs ys = sc capacity * sc durability * sc flavour * sc texture
  where
    sc p = norm $ sum $ zipWith (*) (map p xs) ys
    norm x = if x < 0 then 0 else x

score' :: [Ingredient] -> Cookie -> Int
score' xs ys = sc capacity * sc durability * sc flavour * sc texture * (if sc calories == 500 then 1 else 0)
  where
    sc p = norm $ sum $ zipWith (*) (map p xs) ys
    norm x = if x < 0 then 0 else x
