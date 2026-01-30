import Data.Function
import Data.List

main :: IO ()
main = do
  putStrLn "2025 -- Day 3 -- Lobby"
  contents <- getContents
  let banks = lines contents
  putStrLn ("Part 1: " <> show (part1 banks))
  putStrLn ("Part 2: " <> show (part2 banks))

-- 17524
part1 :: [String] -> Int
part1 = sum . map largestJoltage

-- 
part2 :: [String] -> [Integer]
-- part2 = sum . map (mss 12)
part2 = map (mss 12)

mss :: Int -> String -> Integer
mss b = read . maximum . filter (short b) . segments

short :: Int -> [a] -> Bool
short b xs = (length xs == b)

segments :: [a] -> [[a]]
segments = concatMap inits . tails

{--
inits, tails :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

tails [] = [[]]
tails (x:xs)  = (x:xs) : tails xs
--}

largestJoltage :: String -> Int
largestJoltage xs = read [d1 ,d2]
  where d1 = maximum (init xs)
        (_, xs') = break (== d1) xs 
        d2 = maximum (tail xs')
        
