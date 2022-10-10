import Data.List

main :: IO ()
main = interact solve

solve :: String -> String
solve xs = show $ cont n xs'
  where
    xs' = map (\x -> read x :: Int) $ lines xs
    ys = check 25 xs'
    n = fst $ head $ filter (\(_, y) -> y == False) ys

check :: Int -> [Int] -> [(Int, Bool)]
check n xs = check' n (drop n xs) (take n xs) xs

check' :: Int -> [Int] -> [Int] -> [Int] -> [(Int, Bool)]
check' _ [] _ _ = []
check' n (x : xs) ys (z : zs) = (x, valid x ys) : check' n xs (take n zs) zs

valid :: Int -> [Int] -> Bool
valid n xs = [x + y | x <- xs, y <- xs, x /= y && (x + y) == n] /= []

cont :: Int -> [Int] -> Int
cont n xs = s + l
  where
    xs' = cont' n (filter (< n) xs)
    s = minimum xs'
    l = maximum xs'

cont' :: Int -> [Int] -> [Int]
cont' _ [] = []
cont' n (x : xs) = ys ++ cont' n xs
  where
    xs' = inits (x : xs)
    ys = concat $ filter (\xs -> sum xs == n) xs'
