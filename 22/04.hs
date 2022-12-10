import AOC.List
import Data.List

main :: IO ()
main = do
  c <- getContents
  print $ part1 c
  print $ part2 c

part1 :: String -> Int
part1 = length . filter contains . pairs

part2 :: String -> Int
part2 = length . filter overlaps . pairs

pairs :: String -> [[Int]]
pairs = map (map read . concatMap (splitOn '-') . splitOn ',') . lines

contains :: [Int] -> Bool
contains [a, b, c, d] = if length e1 >= length e2 then f e1 e2 else f e2 e1
  where
    e1 = [a .. b]
    e2 = [c .. d]
    f x y = length (x \\ y) == length x - length y

overlaps :: [Int] -> Bool
overlaps [a, b, c, d] = not (length (e1 \\ e2) == length e1)
  where
    e1 = [a .. b]
    e2 = [c .. d]
