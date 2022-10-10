main :: IO ()
main = interact solve

solve :: String -> String
solve = show . minimum . costs . map read . split ','

costs :: [Int] -> [Int]
costs xs = map (\x -> sum (map (\y -> abs (x - y)) xs)) xs

split :: Char -> String -> [String]
split c = words . map (\x -> if x == c then ' ' else x)
