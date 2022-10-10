main :: IO ()
main = interact solve

solve :: String -> String
solve = show . countInc 0 . map read . lines

countInc :: Int -> [Int] -> Int
countInc n (y : []) = n
countInc n (x : y : xs) = if y > x then countInc (n + 1) (y : xs) else countInc n (y : xs)
