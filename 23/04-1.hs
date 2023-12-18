import Data.List

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . map points . nums . lines

nums :: [String] -> [([Int],[Int])]
nums = map (\s -> let s' = dropWhile (/=':') s
                      (s1,s2) = break (=='|') s'
                      xs = map read (words (drop 2 s1))
                      ys = map read (words (drop 2 s2)) in
                    (xs,ys))

points :: ([Int],[Int]) -> Int
points (xs,ys) = if l == 0 then 0 else 2^(l-1)
  where l = length (intersect xs ys)
