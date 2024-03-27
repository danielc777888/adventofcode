module Aoc.List
  ( chunk,
    splitOn,
  )
where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs
  | null b = [a]
  | otherwise = a : splitOn x (drop 1 b)
  where
    (a, b) = break (== x) xs

chunk :: Int -> [b] -> [[b]]
chunk _ [] = []
chunk x xs = take x xs : chunk x (drop x xs)
