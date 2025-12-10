import Data.List

main :: IO ()
main = do
  putStrLn "2025 -- Day 1 -- Secret Entrance"
  contents <- getContents
  let input = lines contents
  putStrLn ("Part 1: " <> show (part1 input))
  putStrLn ("Part 2: " <> show (part2 input))

-- 1018
part1 :: [String] -> Int
part1  = snd . foldl rotate (50, 0)
--rotate 50

-- 
part2 :: [String] -> Int
part2 xs = 1

rotate :: (Int, Int) -> String -> (Int, Int)
rotate (p, s) (x:xs) 
  | x == 'L' = if lp == 0 then (lp, s + 1) else (lp, s)
  | x == 'R' =  if rp == 0 then (rp, s + 1) else (rp, s)
  where lp = rotateLeft p p'
        rp = rotateRight p p'
        p' = read xs

rotateLeft :: Int -> Int -> Int
rotateLeft x y = (x - y) `mod` 100

rotateRight :: Int -> Int -> Int
rotateRight x y = (x + y) `mod` 100
