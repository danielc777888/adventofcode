import Data.List (permutations)

type Triangle = [Int]

main :: IO()
main = interact solve

solve :: String -> String
solve = show . length . filter id .  map (valid . permutations . triangle) . lines

valid :: [Triangle] -> Bool
valid [] = True
valid ([x,y,z]:xs) = if x + y > z then valid xs else False

triangle :: String -> Triangle
triangle xs = [read (xs'!!0), read (xs'!!1), read (xs'!!2)]
  where xs' = words xs
