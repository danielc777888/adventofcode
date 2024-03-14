-- 2046

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . foldr (\x acc -> acc + (length (enclose (encode x)) - length x)) 0 . lines

encode :: [Char] -> [Char]
encode [] = []
encode (x : xs)
  | x == '\\' = '\\' : '\\' : encode xs
  | x == '"' = '\\' : '"' : encode xs
  | otherwise = x : encode xs

enclose :: String -> String
enclose xs = "\"" ++ xs ++ "\""
