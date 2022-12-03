import AOC.List
import Data.Char
import Data.List

main :: IO ()
main = do
  c <- getContents
  print $ part1 c
  print $ part2 c

part1 :: String -> Int
part1 = sum . map (priority . common) . lines

part2 :: String -> Int
part2 = sum . map (priority . common') . chunk 3 . lines

common :: String -> Char
common xs = head $ a \\ (a \\ b)
  where
    (a, b) = splitAt (length xs `div` 2) xs

common' :: [String] -> Char
common' [xs, ys, zs] = head [x | x <- xs, y <- ys, z <- zs, x == y && y == z]

priority :: Char -> Int
priority c = if isLower c then ord c - 96 else ord c - 38
