-- 1333

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . foldr (\x acc -> acc + (length x - length (strData x))) 0 . lines

strData :: [Char] -> [Char]
strData [] = []
strData (x : []) = []
strData (x : y : xs)
  | x == '\\' && y == '\\' = '\\' : strData xs
  | x == '\\' && y == '"' = '"' : strData xs
  | x == '\\' && y == 'x' = '\'' : strData (drop 2 xs)
  | x == '"' = strData (y : xs)
  | otherwise = x : strData (y : xs)
