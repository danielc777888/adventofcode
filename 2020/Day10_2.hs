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


--TODO write own performance subsequences functionsx

subs :: [Int] -> [[Int]]
subs [] = [[]]
subs [x] = [[x], []]
subs (x:xs) = is ++ ts' ++ rs ++ rs'
  where is = initSubs (x:xs)
        ts = filter (\xs -> length xs >= 3) is
        ts'= map (\xs -> [head xs, last xs]) ts
        ts'' = filter (\xs -> length xs >= 3) is
        rs = concat $ map (\xs -> remSubs xs) ts''
        rs' = concat $ map (\xs -> map reverse (remSubs (reverse xs))) ts''
       
initSubs :: [Int] -> [[Int]]
initSubs [] = [[]]
initSubs (x:xs) = tail (inits (x:xs)) ++ initSubs xs

remSubs :: [Int] -> [[Int]]
remSubs [] = [[]]
remSubs [x] = [[]]
remSubs (x:y:[]) = [[]]
remSubs (x:y:z:[]) = [[]] 
remSubs (x:y:z:xs) = [s1] ++ [s2] ++ [s3] ++ remSubs s1 ++ remSubs s2 ++ remSubs s3
  where s1 = x:y:xs
        s2 = x:z:xs
        s3 = x:xs
        
