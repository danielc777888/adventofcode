import Data.List

main :: IO()
main = interact solve

solve :: String -> String
solve xs = show (length as)
           where xs' = sort $ map (\x -> read x::Int) $ lines xs
                 xs'' = [0] ++ xs' ++ [last xs' + 3]
                 rs = remove [0] xs'
                 as = arrangements xs' rs
                 bs = filter (\x -> x `notElem` rs) xs'

arrangements :: [Int] -> [Int] -> [[Int]]
arrangements xs ys = filter (\xs -> valid (sort (xs++bs)))  (subs ys)
   where bs = [0] ++ filter (\x -> x `notElem` ys) xs

--candidates :: [Int] -> [[Int]] -> [[Int]]
--candidates [] xs = xs
--candidates xs xss = if valid xs then candidates (generate xs) (xs:xss) else candidates xs

generate :: [Int] -> [[Int]]
generate (x:y:z:xs) = if z - x < 3 then [x:z:xs] ++ generate (x:z:xs) else generate (y:z:xs)
generate (x:y:[]) = [[]]
generate _ = [[]]


remove :: [Int] -> [Int] ->  [Int]
remove _ [x] = []
remove xs (y:ys) = if valid (xs ++ ys) then [y] ++ remove (xs ++ [y]) ys else remove (xs ++ [y]) ys

validate :: ([[Int]], Int) -> [[Int]]
validate (xss, y) = filter(\xs -> valid ([0] ++ xs ++ [y])) xss

valid :: [Int] -> Bool
valid (y:[]) = True
valid (x:y:xs) = if (y - x) > 3 then False else valid (y:xs)


subs :: [Int] -> [[Int]]
subs [] = [[]]
subs (x:xs) = subs xs ++ map (x:) (subs xs)

subs2 :: [Int] -> [[Int]]        
subs2 = foldr tstep [[]]

tstep :: Int -> [[Int]] -> [[Int]]
tstep x ([]:xss) = []:search x [] xss
                where search x xs [] = [x:xs]
                      search x xs (ys:xss)
                        | (head ys - x) >= 3 = ys:search x ys xss
                        | otherwise = (x:xs):xss