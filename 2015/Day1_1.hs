solve :: String -> Int
solve [] = 0
solve (x:xs) = if x == '(' then 1 + solve xs else solve xs -1 

main :: IO()
main = interact (show . solve)
