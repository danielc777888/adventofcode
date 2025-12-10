import Data.List

main :: IO ()
main = do
  putStrLn "2025 -- Day 1 -- Secret Entrance"
  contents <- getContents
  let input = parse contents
  putStrLn ("Part 1: " <> show (part1 input))
  putStrLn ("Part 2: " <> show (part2 input))

parse :: String -> [String]
parse = lines

-- 1018
part1 :: [String] -> Int
part1  = rotate 50

-- 
part2 :: [String] -> Int
part2 xs = 1

rotate :: Int -> [String] -> Int
rotate _ [] = 0
rotate x (y:ys)
  | "L" `isPrefixOf` y = let x' = rotateLeft x (read (tail y)) in if x' == 0 then 1 + rotate x' ys else rotate x' ys
  | "R" `isPrefixOf` y = let x' = rotateRight x (read (tail y)) in if x' == 0 then 1 + rotate x' ys else rotate x' ys
  | otherwise = error ("Unknown pattern: " <> y)

rotateLeft :: Int -> Int -> Int
rotateLeft x y = (x - y) `mod` 100

rotateRight :: Int -> Int -> Int
rotateRight x y = (x + y) `mod` 100
