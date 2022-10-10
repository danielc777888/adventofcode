main :: IO ()
main = interact solve

solve :: String -> String
solve = show . uncurry (*) . rates . lines

rates :: [String] -> (Int, Int)
rates (x : xs) = (toDec g, toDec e)
  where
    cs = foldr count (replicate (length x) 0) (x : xs)
    g = map (\x -> if x >= 0 then 1 else 0) cs
    e = map (\x -> if x < 0 then 1 else 0) cs

count :: [Char] -> [Int] -> [Int]
count [] _ = []
count (x : xs) (y : ys) = if x == '1' then (1 + y) : count xs ys else (y - 1) : count xs ys

toDec :: [Int] -> Int
toDec = foldr (\x y -> x + 2 * y) 0 . reverse
