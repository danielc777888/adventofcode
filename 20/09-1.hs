main :: IO ()
main = interact solve

solve :: String -> String
solve = show . head . filter (\(_, y) -> y == False) . check 25 . map (\x -> read x :: Int) . lines

check :: Int -> [Int] -> [(Int, Bool)]
check n xs = check' n (drop n xs) (take n xs) xs

check' :: Int -> [Int] -> [Int] -> [Int] -> [(Int, Bool)]
check' _ [] _ _ = []
check' n (x : xs) ys (z : zs) = (x, valid x ys) : check' n xs (take n zs) zs

valid :: Int -> [Int] -> Bool
valid n xs = [x + y | x <- xs, y <- xs, x /= y && (x + y) == n] /= []
