-- 1268

import Data.Char

main :: IO ()
main = interact solve

solve :: String -> String
solve xs = show $ solve' 0 xs (zip [0..] xs)

solve' :: Int -> String -> [(Int, Char)] -> Int
solve' n _ (_:[]) = n
solve' n xs (y:ys) = if snd y == y' then solve' (n + digitToInt (snd y)) xs ys
                     else solve' n xs ys
          where l = length xs
                s = l `div` 2
                y' = xs !! ((fst y + s) `mod` l)
