-- TODO: Look into using infinite list with cycles.

import Data.Char
import Data.List

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . concat . map show . play . map digitToInt . init

play :: [Int] -> [Int]
play [] = []
play xs = play' 100 (length xs) xs

play' :: Int -> Int -> [Int] -> [Int]
play' 0 l xs = tail (dropWhile (/= 1) xs) ++ takeWhile (/= 1) xs
play' n l (x : xs) = play' (n - 1) l xs'
  where
    pu = take 3 xs
    tpu = drop 3 xs
    (lt, gt) = partition (< x) tpu
    d' = if null lt then maximum gt else maximum lt
    xs' = takeWhile (/= d') tpu ++ [d'] ++ pu ++ tail (dropWhile (/= d') tpu) ++ [x]
