
main :: IO()
main = interact solve

solve :: String -> String
solve = show . length . filter snd . map(\x -> (x, tls x)) . lines
--solve = show . map tls . lines

tls :: String ->  Bool
tls xs = go 0 xs False False
  where go ib [] i o = if i && not o then True else False
        go ib (x:xs) i o
          | x == '[' = go (ib+1) xs i o
          | x == ']' = go (ib-1) xs i o
          | isPalindrome xs' && ib == 0 = go ib xs True o
          | isPalindrome xs' && ib > 0 = go ib xs i True
          | otherwise = go ib xs i o
         where xs' = take 4 (x:xs)

isPalindrome :: String -> Bool
isPalindrome s@[a,b,c,d] = s == (reverse s) && a /= b
isPalindrome _ = False
