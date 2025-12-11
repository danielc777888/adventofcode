
main :: IO ()
main = do
  putStrLn "2025 -- Day 1 -- Secret Entrance"
  contents <- getContents
  let input = lines contents
  putStrLn ("Part 1: " <> show (part1 input))
  putStrLn ("Part 2: " <> show (part2 input))

-- 1018
part1 :: [String] -> Int
part1  = (\(_, _, x) -> x) . foldl rotate (50, 0, 0)

-- 5815
part2 :: [String] -> Int
part2 = (\(_, x, _) -> x) . foldl rotate (50, 0, 0)

rotate :: (Int, Int, Int) -> String -> (Int, Int, Int)
rotate (p, s, sp) (x:xs) 
  | x == 'L' = (lp, s + ld, sp + lps)
  | x == 'R' = (rp, s + rd, sp + rps)
  where (lp, ld, lps) = rotateL p p' p (0, 0)
        (rp, rd, rps) = rotateR p p' p (0, 0)
        p' = read xs

rotateL :: Int -> Int -> Int -> (Int, Int) -> (Int, Int, Int)
rotateL s e c (z, zp) 
  | e == 0 = (c, z, if c == 0 then zp + 1 else zp)
  | c == 0 = rotateL s (e - 1) c' (z + 1, zp)
  | otherwise = rotateL s (e - 1) c' (z, zp)
  where c' = if (c - 1) == (-1) then 99 else (c - 1)

rotateR :: Int -> Int -> Int -> (Int, Int) -> (Int, Int, Int)
rotateR s e c (z, zp) 
  | e == 0 = (c, z, if c == 0 then zp + 1 else zp)
  | c == 0 = rotateR s (e - 1) c' (z + 1, zp)
  | otherwise = rotateR s (e - 1) c' (z, zp)
  where c' = if (c + 1) == 100 then 0 else (c + 1)
