import Aoc.Loop (apply)
import Data.Char

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . length . apply 50 lookAndSay . head . lines

lookAndSay :: [Char] -> [Char]
lookAndSay = f 0
  where
    f n (y : []) = intToDigit (n + 1) : y : []
    f n (x : y : ys)
      | x == y = f (n + 1) (y : ys)
      | otherwise = intToDigit (n + 1) : x : (f 0 (y : ys))
