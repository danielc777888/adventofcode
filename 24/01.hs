-- (2031679,19678534)

import Data.List (sort)

main :: IO ()
main = interact solve

solve :: String -> String
solve input = show (part1 input', part2 input')
  where input' = map (map read . words) (lines input)

part1 :: [[Int]] -> Int
part1 xss = sum l4
  where l1 = map (!! 0) xss
        l2 = map (!! 1) xss
        l3 = zip (sort l1) (sort l2)
        l4 = map (\(x, y) -> abs (x - y)) l3

part2 :: [[Int]] -> Int
part2 xss = sum l3
  where l1 = map (!! 0) xss
        l2 = map (!! 1) xss
        l3 = map (\x -> x * length (filter (==x) l2)) l1
