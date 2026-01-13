{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TI

type Range = (Integer, Integer)

main :: IO ()
main = do
  putStrLn "2025 -- Day 2 -- Gift Shop"
  contents <- TI.getContents
  let ranges = parse contents
  print ranges
  putStrLn ("Part 1: " <> show (part1 ranges))
  putStrLn ("Part 2: " <> show (part2 ranges))

parse :: T.Text -> [Range]
parse = map (\y -> let [f, t] = T.splitOn "-" y
                       in (read (T.unpack f), read (T.unpack t))) .  T.splitOn ","

-- 24747430309
part1 :: [Range] -> Integer
part1 = sum . concatMap (\(x, y) -> filter invalid [x .. y])

-- 30962646823
part2 :: [Range] -> Integer
part2 = sum . concatMap (\(x, y) -> filter invalid' [x .. y])

invalid :: Integer -> Bool
invalid x = xs == ys
  where
    x' = show x 
    mid = length x' `div` 2
    (xs, ys) = splitAt mid x'

invalid' :: Integer -> Bool
invalid' x = any (\c -> length c == 1) cs
  where
    x' = show x 
    mid = length x' `div` 2
    cs = map (nub . chunks x') [1..mid]

chunks :: [Char] -> Int -> [String]
chunks [] _ = []
chunks xs c = take c xs : chunks (drop c xs) c
