main :: IO ()
main = interact solve

solve :: String -> String
solve = show . solve' . map (\x -> read x :: Int) . lines

solve' :: [Int] -> Int
solve' xs = head [x * y * z | x <- xs, y <- xs, z <- xs, y + x + z == 2020]
