main :: IO ()
main = interact solve

solve :: String -> String
solve = show . countInc 0 . sums . map read . lines

countInc :: Int -> [Int] -> Int
countInc n (y : []) = n
countInc n (x : y : xs) = if y > x then countInc (n + 1) (y : xs) else countInc n (y : xs)

sums :: [Int] -> [Int]
sums (x : y : z : xs) = (sum [x, y, z]) : sums (y : z : xs)
sums _ = []
