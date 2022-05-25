ex1 = ")())())"

solve :: String -> Int
solve [] = 0
solve (x : xs) = if x == '(' then 1 + solve xs else solve xs -1

main :: IO ()
main = interact (show . solve . head . lines)

{-- main = do
let a = solve ex1
putStrLn (show a)
--}
