main :: IO ()
main = interact solve

solve :: String -> String
solve = show . solve' . map (\x -> read x :: Int) . lines

solve' :: [Int] -> Int
solve' xs = head [x * y | x <- xs, y <- xs, y + x == 2020]
