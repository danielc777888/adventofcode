import Data.List

main :: IO()
main = interact solve

solve :: String -> String
solve xs = show (length as)
           where xs' = sort $ map (\x -> read x::Int) $ lines xs
                 rs = remove [0] xs'
                 as = arrangements xs' rs
                 bs = filter (\x -> x `notElem` rs) xs'

arrangements :: [Int] -> [Int] -> [[Int]]
arrangements xs ys = filter (\xs -> valid (sort (xs++bs)))  (subs ys)
   where bs = [0] ++ filter (\x -> x `notElem` ys) xs


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
        
