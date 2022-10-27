import Data.Char
import Data.List
import Data.Maybe

main :: IO()
main = do
  contents <- getContents
  let input :: (Int, Int) = case words (filter (not . isPunctuation) (filter (not . isAlpha) contents)) of
                [a, b] -> (read a, read b)
  print $ code 20151125 (findN input) 1

findN :: (Int, Int) -> Int
findN (a, b) = findN' (a, b) 1 1

findN' :: (Int, Int) -> Int -> Int -> Int
findN' (a, b) d n = if isJust r then fst $ fromJust r else findN' (a, b) (d+1) (n+d)
  where ds = zip [n..n+d] $ zip  [d,d-1..1] [1..d]
        r = find (\(n, rc) -> rc == (a, b)) ds

code :: Int -> Int -> Int -> Int
code c n i
  | n == i = c
  | otherwise = code c' n (i+1)
    where c' = (c * 252533) `rem` 33554393
