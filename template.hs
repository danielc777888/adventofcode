import AOC.PlumbingCombinator (fork)

main :: IO ()
main = interact $ show . fork (solve1, solve2)

solve1 :: String -> String
solve1 = id

solve2 :: String -> String
solve2 = id
