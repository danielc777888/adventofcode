import Data.List (transpose,sort,sortBy)

main :: IO()
main = interact solve

solve :: String -> String
solve = map (snd . head . sort . runCounts . sort) . transpose . lines

runCounts :: [Char] -> [(Int,Char)]
runCounts = go 1
  where go c (x:[]) = [(c,x)]
        go c (x:y:xs)
          | x == y = go (c+1) (y:xs)
          | otherwise = (c,x):go 1 (y:xs)
