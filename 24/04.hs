import Data.List
import Data.Maybe
import Data.Char

main :: IO ()
main = do
  putStrLn "2024 -- Day 4 -- Ceres Search"
  contents <- getContents
  let input = parse contents
  putStrLn ("Part 1: " <> output (part1 input))
  putStrLn ("Part 2: " <> part2 input)

parse :: String -> [[Char]]
parse = lines

word :: String
word = "XMAS"

li = 139

-- 2639
part1 :: [[Char]] -> Int
part1 xss = sum $ concat [rows, rowsB, cols, colsB, diagLR, diagLRB, diagRL, diagRLB]
  where
    rows = map count xss
    rowsB = map (count . reverse) xss
    cols = map count (transpose xss)
    colsB = map (count . reverse) (transpose xss)
    diagLR = map count $ diags' xss
    diagLRB = map (count . reverse) $ diags' xss
    diagRL = map count (diags xss)
    diagRLB = map (count . reverse) (diags xss)

-- right to left
diags :: [[Char]] -> [[Char]]
diags xss = map (\xs -> map (\(x,y) -> (xss !! x) !! y) xs) ds
  where
    start = [[(0, 0)]]
    tIdxs = [zip [x, x - 1..0] [0..x] | x <- [1..(li - 1)]]
    cIdx = [zip [li,(li - 1)..0] [0..li]]
    bIdxs = [zip [li,(li - 1)..x] [x..li] | x <- [1..(li - 1)]]
    end = [[(li, li)]]
    ds = concat [start, tIdxs, cIdx, bIdxs, end]

-- left to right
diags' :: [[Char]] -> [[Char]]
diags' xss = map (\xs -> map (\(x,y) -> (xss !! x) !! y) xs) ds
  where
    start = [[(0, li)]]
    tIdxs = [zip [0..x] [li-x..li] | x <- [1..(li - 1)]]
    cIdx = [zip [0..li] [0..li]]
    bIdxs = [zip [li-x..li] [0..x] | x <- [1..(li - 1)]]
    end = [[(li, 0)]]
    ds = concat [start, tIdxs, cIdx, bIdxs, end]

-- 
part2 :: [[Char]] -> [Char]
part2 = undefined

output :: Int -> String
output = show

count :: [Char] -> Int
count [] = 0
count w@(x:xs) = if word `isPrefixOf` w then 1 + count xs else count xs
