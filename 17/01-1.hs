-- 1069

import Data.Char

main :: IO ()
main = interact solve

solve :: String -> String
solve c@(x:xs) = show $ go 0 c'
  where c' = c ++ [x]
        go n (_:[]) = n
        go n (x:y:xs) = if x == y then go (n + digitToInt x) (y:xs)
                        else go n (y:xs)
