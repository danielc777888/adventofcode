-- 1638

import Data.List

main :: IO ()
main = interact $ solve

solve :: String -> String
solve = show . length . filter ((== 150) . sum) . subsequences . map read . lines

minContainers :: [[Int]] -> [[Int]]
minContainers xss = filter ((== min) . length) xss
  where
    min = minimum (map length xss)
