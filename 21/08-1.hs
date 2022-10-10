type Digit = String

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . counts . concat . map digits . lines

digits :: String -> [Digit]
digits = words . drop 2 . dropWhile (/= '|')

counts :: [Digit] -> Int
counts [] = 0
counts (x : xs)
  | l == 2 || l == 3 || l == 7 || l == 4 = 1 + counts xs
  | otherwise = counts xs
  where
    l = length x
