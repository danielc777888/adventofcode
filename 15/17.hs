import Data.List
import AOC.PlumbingCombinator (fork)

main :: IO()
main = interact $ show . fork (solve1, solve2)

solve1 :: String -> String
solve1 = show . length . filter ( (== 150) . sum ) . subsequences . map read . lines

solve2 :: String -> String
solve2 = show . length . minContainers . filter ( (== 150) . sum ) . subsequences . map read . lines

minContainers :: [[Int]] -> [[Int]]
minContainers xss = filter ( (== min) . length) xss
  where min = minimum (map length xss)
