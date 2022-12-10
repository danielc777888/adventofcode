import Data.Char
import Data.List
import Data.Array

main :: IO ()
main = do
  c <- getContents
  print $ part1 c

-- print $ part2 c

part1 :: String -> Int
part1 = treesVisible . map (map digitToInt) . lines

part2 :: String -> String
part2 = id

treesVisible :: [[Int]] -> Int
treesVisible rows = length visibleTrees
    where cols = transpose rows
          trees = [ ((fst xs, fst x), snd x) | xs <- (zip [1..] rows), x <- (zip [1..] (snd xs))]
          rowsMax = listArray (1, length rows) (maxTrees rows)
          colsMax = listArray (1, length cols) (maxTrees cols)
          edgeTrees = length rows * 2 + length cols * 2 - 4
          visibleTrees = filter (\t -> isVisible t rowsMax colsMax) trees


maxTrees :: [[Int]] -> [(Int, [Int])]
maxTrees xss = map (\r -> let mr = maximum r in (mr, map fst (filter (\(c, v) -> v == mr)(zip [1..] r))))  xss

isVisible :: ((Int, Int), Int) -> Array Int (Int, [Int]) -> Array Int (Int, [Int]) -> Bool
isVisible ((r, c), t) rs cs = isEdge bsr bsc (r, c) || (t == rm && (c < minimum rmc' || c > maximum rmc')) || (t == cm && (r < minimum cmr' || r > maximum cmr'))
    where (rm, rmc) = rs!r
          rmc' = rmc \\ [r]
          (cm, cmr) = cs!c
          cmr' = cmr \\ [c]
          bsr = bounds rs
          bsc = bounds cs

isEdge :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isEdge (r1, r2) (c1, c2) (r, c) = r == r1 || r == r2 || c == c1 || c == c2