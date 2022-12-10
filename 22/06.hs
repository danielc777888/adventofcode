import Data.List

main :: IO ()
main = do
  c <- getContents
  print $ part1 c
  print $ part2 c

part1 :: String -> Int
part1 = firstMarker 4 0

part2 :: String -> Int
part2 = firstMarker 14 0

firstMarker :: Int -> Int -> String -> Int
firstMarker _ m [] = m
firstMarker n m xs
    | length (nub xs') == length xs' = m + n
    | otherwise = firstMarker n (m+1) (drop 1 xs)
    where xs' = take n xs