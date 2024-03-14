-- timeout

import Data.Function
import Data.List
import Data.Ord

main :: IO ()
main = do
  contents <- getContents
  print $ solve contents

example :: [Int]
example = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11]

solve :: String -> [Int]
solve s = minimumBy minBy $ nub $ validGroups w ps
  where ps = map read (lines s)
        w = sum ps `div` 3

minBy :: [Int] -> [Int] -> Ordering
minBy x y
  | length x < length y = LT
  | quantEnt x < quantEnt y = LT
  | length x > length y = GT
  | quantEnt x > quantEnt y = GT
  | otherwise = EQ

quantEnt :: [Int] -> Int
quantEnt = product

validGroups :: Int -> [Int] -> [[Int]]
validGroups w xs = map snd $ filter fst $ map (valid w (0,0,0) ([], [], [])) $ take 1000000000 $ permutations (reverse xs)

valid :: Int -> (Int, Int, Int) -> ([Int], [Int], [Int]) -> [Int] -> (Bool, [Int])
valid w (s1, s2, s3) (g1, _, _) [] = if s1 == w && s2 == w && s3 == w then (True, g1) else (False, [])
valid w (s1, s2, s3) (g1, g2, g3) (x:xs)
  | s1 > w || s2 > w || s3 > w = (False, [])
  | s1 < w = valid w (s1 + x, s2, s3) (x:g1, g2, g3) xs
  | s1 == w && s2 < w = valid w (s1, s2 + x, s3) (g1, x:g2, g3) xs
  | s2 == w && s3 < w = valid w (s1, s2, s3 + x) (g1, g2, x:g3) xs
  | otherwise = error "should not be here"
