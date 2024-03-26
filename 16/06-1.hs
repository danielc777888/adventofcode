-- qoclwvah

import Data.List (sort, sortBy, transpose)

main :: IO()
main = interact solve

solve :: String -> String
solve = map (snd . head . sortBy (\x y -> compare y x) . runCounts 1 . sort) . transpose . lines

runCounts :: Int -> [Char] -> [(Int,Char)]
runCounts c (x:[]) = [(c,x)]
runCounts c (x:y:xs) = if (x == y) then runCounts (c+1) (y:xs) else  (c,x):runCounts 1 (y:xs)
