solve :: String -> Int
solve = length . head . dropWhile (\x -> (sum x) >= 0) . reverse . inits . map (\x -> if x == '(' then 1 else -1)

inits :: [a] -> [[a]]
inits [x] = [[x]]
inits xs = [xs] ++ inits (init xs)

main :: IO ()
main = interact (show . solve)
