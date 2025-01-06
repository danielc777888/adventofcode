

main :: IO ()
main = do
  putStrLn "2024 -- Day 2 -- Red-Nosed Reports"
  contents <- getContents
  let input = parse contents
  putStrLn ("Part 1: " <> output (part1 input))
  putStrLn ("Part 2: " <> output (part2 input))

parse :: String -> [[Int]]
parse = map (map read . words) . lines

-- 287
part1 :: [[Int]] -> Int
part1 = length . filter safe

-- 354
part2 :: [[Int]] -> Int
part2 = length . filter (\xs -> if safe xs then True else any safe (gen xs))

output :: Int -> String
output = show

safe :: [Int] -> Bool
safe = go 0
  where go d (x:y:xs)
          | abs (x - y) < 1 || abs (x - y) > 3 = False
          | x < y && d < 0 = False
          | y < x && d > 0 = False
          | otherwise = go (y - x) (y:xs)
        go _ _ = True

gen :: [Int] -> [[Int]]
gen xs = foldr (\i acc ->
                  let (l1, l2) = splitAt i xs
                  in  (l1 ++ drop 1 l2):acc) [] [0..(length xs - 1)]
