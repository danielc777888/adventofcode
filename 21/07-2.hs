main :: IO ()
main = interact solve

solve :: String -> String
solve = show . minimum . costs . map read . split ','

costs :: [Int] -> [Int]
costs xs = map (cost xs) [0 .. maximum xs]

cost :: [Int] -> Int -> Int
cost xs x = sum $ map (\y -> sum [0 .. abs (y - x)]) xs

split :: Char -> String -> [String]
split c = words . map (\x -> if x == c then ' ' else x)
