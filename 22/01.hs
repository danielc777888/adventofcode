-- TEST
-- ANOTHER TEST

import AOC.List
import Data.List

main :: IO ()
main = do
  contents <- getContents
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 = maximum . map (sum . map read) . splitOn "" . lines

part2 :: String -> Int
part2 = sum . take 3 . sortBy (flip compare) . map (sum . map read) . splitOn "" . lines
