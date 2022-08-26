import Data.Char

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . countNumbers 0 (+) . head . lines

countNumbers :: Int -> (Int -> Int -> Int) -> String -> Int
countNumbers n f (x : []) = n
countNumbers n f (x : y : xs)
  | isDigit x = countNumbers n' (+) xs'
  | x == '-' = countNumbers n (-) (y : xs)
  | otherwise = countNumbers n f (y : xs)
  where
    num = read (takeWhile isDigit (x : y : xs))
    xs' = dropWhile isDigit (x : y : xs)
    n' = f n num
