
main :: IO ()
main = do
  putStrLn "2025 -- Day 3 -- Lobby"
  contents <- getContents
  let banks = lines contents
  putStrLn ("Part 1: " <> show (part1 banks))
  -- putStrLn ("Part 2: " <> show (part2 ranges))

-- 
part1 :: [String] -> Int
part1 = sum . map largestJoltage

-- 


largestJoltage :: String -> Int
largestJoltage xs = read [d1 ,d2]
  where d1 = maximum (init xs)
        (_, xs') = break (== d1) xs 
        d2 = maximum (tail xs')
        
