import Data.List

main :: IO ()
main = interact solve

type Answers = [Char]

type Group = [Answers]

solve :: String -> String
solve = show . sum . map counts . groups . lines

groups :: [String] -> [Group]
groups [] = []
groups xs = [x] ++ if null y then [] else groups (tail y)
  where
    (x, y) = break null xs

counts :: Group -> Int
counts g = length $ answers (length g) $ sort $ filter (/= '\n') $ unlines g

answers :: Int -> Answers -> Answers
answers _ [] = []
answers n (x : xs) = if n == length ys then x : answers n xs' else answers n xs'
  where
    ys = x : takeWhile (== x) xs
    xs' = dropWhile (== x) xs
