import Data.List

main :: IO()
main = interact solve

solve :: String -> String
solve = show . conquer . divide . adapters

adapters :: String -> [Int]
adapters = sort . map (\x -> read x::Int) . lines

arrangements :: [Int] -> [Int] -> [[Int]]
arrangements xs ys = filter (\xs -> valid (sort (xs++bs)))  (subs ys)
   where bs = [0] ++ filter (\x -> x `notElem` ys) xs

divide :: [Int] -> ([Int], [[Int]])
divide xs = (xs, divide' [] xs)
    where divide' ys (x:y:z:[]) = [ys ++ [x, y, z]]
          divide' ys (x:y:xs) = if (y - x) == 3 && (length ys) > 1 then [ys ++ [x]] ++ divide' [] (y:xs) else divide' (ys ++ [x]) (y:xs)

conquer :: ([Int], [[Int]]) -> Int
conquer (xs, xss) = foldl (\acc ys -> if (head ys == 1) then (narrangements 0 xs ys) * acc else (narrangements (head ys - 3) xs ys) * acc) 1 xss

narrangements :: Int -> [Int] -> [Int] -> Int
narrangements x xs ys = length (arrangements xs (remove [x] ys))

remove :: [Int] -> [Int] ->  [Int]
remove _ [x] = []
remove xs (y:ys) = if valid (xs ++ ys) then [y] ++ remove (xs ++ [y]) ys else remove (xs ++ [y]) ys

valid :: [Int] -> Bool
valid (y:[]) = True
valid (x:y:xs) = if (y - x) > 3 then False else valid (y:xs)

subs :: [Int] -> [[Int]]
subs [] = [[]]
subs (x:xs) = subs xs ++ map (x:) (subs xs)
