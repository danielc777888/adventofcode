--TODO: Look into using infinite list with cycles.
--TODO: Make more efficient, takes 100s for 1000 turns, must complete 10000000 turns.

import Data.Char
import Data.List

main :: IO()
main = interact solve

solve :: String -> String
solve = show . play . map digitToInt . init

play :: [Int] -> Int
play [] = 0
play xs = play' 1000 (length xs') xs'
  where xs' = xs ++ [(maximum xs)+1..1000000]

play' :: Int -> Int -> [Int] -> Int
play' 0 l xs = product (tail (take 3 (dropWhile (/=1) xs)))
play' n l (x:xs) = play' (n-1) l xs'
  where pu = take 3 xs
        tpu = drop 3 xs
        (lt, gt) = partition (<x) tpu
        d' = if null lt then maximum gt else maximum lt
        xs' = takeWhile (/=d') tpu ++ [d'] ++ pu ++ tail (dropWhile (/=d') tpu) ++ [x]
