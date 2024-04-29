-- 34925

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . map (minMaxDiff . map read . words) . lines
-- solve = show . sum . map minMaxDiff . grid Int
-- grid :: ??? -> String -> Grid ???

minMaxDiff :: [Int] -> Int
minMaxDiff xs = maximum xs - minimum xs
