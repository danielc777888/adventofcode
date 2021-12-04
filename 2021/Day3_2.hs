import Data.Char

main :: IO()
main = interact solve

solve :: String -> String
solve = show . uncurry (*) . rates . lines

rates :: [String] -> (Int, Int)
rates (x:xs) = (toDec o, toDec c)
    where cs = counts (x:xs)
          o = map digitToInt (ox (x:xs) (0, cs))
          c = map digitToInt (carb (x:xs) (0, cs))

ox :: [String] -> (Int, [Int]) -> String
ox (x:[]) _ = x
ox (x:xs) (i, cs) = if cs!!i >= 0 then ox f1 (i+1, counts f1) else ox f0 (i+1, counts f0)
    where f1 = filterNums i '1' (x:xs)
          f0 = filterNums i '0' (x:xs)

carb :: [String] -> (Int, [Int]) -> String
carb (x:[]) _ = x
carb (x:xs) (i, cs) = if cs!!i >= 0 then carb f0 (i+1, counts f0) else carb f1 (i+1, counts f1)
    where f1 = filterNums i '1' (x:xs)
          f0 = filterNums i '0' (x:xs)

filterNums :: Int -> Char -> [String] -> [String]
filterNums i b = filter(\n -> n!!i == b)

counts :: [String] -> [Int]
counts (x:xs) = foldr count (replicate (length x) 0) (x:xs)

count :: [Char] -> [Int] -> [Int]
count [] _ = []
count (x:xs) (y:ys) = if x == '1' then (1 + y):count xs ys else (y - 1):count xs ys

toDec :: [Int] -> Int
toDec = foldr (\x y -> x + 2*y) 0 . reverse
