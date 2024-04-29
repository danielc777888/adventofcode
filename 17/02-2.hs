-- 221

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . map (calc . map read . words) . lines

calc :: [Int] -> Int
calc xs = calc' xs xs

calc' :: [Int] -> [Int] -> Int
calc' [] _ = 0
calc' (x:xs) ys = if x' > 0 then x' else calc' xs ys
  where x' = foldl (\acc e -> let (d,m) = divMod x e in
                       if x > e && m == 0 then d else acc) 0 ys
