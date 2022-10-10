import Data.List

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . mult . counts . diffs . sort . map read . lines

diffs :: [Int] -> [Int]
diffs (x : xs) = diffs' (0 : x : xs)

diffs' :: [Int] -> [Int]
diffs' (y : []) = [3]
diffs' (x : y : xs) = (y - x) : diffs' (y : xs)

counts :: [Int] -> (Int, Int)
counts xs = (ones, threes)
  where
    ones = length (filter (== 1) xs)
    threes = length (filter (== 3) xs)

mult :: (Int, Int) -> Int
mult (x, y) = x * y
