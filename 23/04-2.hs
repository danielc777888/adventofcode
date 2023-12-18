import Data.List
import Data.Array

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . wins . nums . lines

nums :: [String] -> [([Int],[Int])]
nums = map (\s -> let s' = dropWhile (/=':') s
                      (s1,s2) = break (=='|') s'
                      xs = map read (words (drop 2 s1))
                      ys = map read (words (drop 2 s2)) in
                    (xs,ys))

wins :: [([Int],[Int])] -> Int
wins s = wins' 1 (array (0,length s) (zip [0..] s)) s

wins' :: Int -> Array Int ([Int],[Int]) -> [([Int],[Int])] -> Int
wins' _ _ [] = 0
wins' i s (x:xs) = 1 + wins' (i+1) s xs + if length ws == 0 then 0 else ts
        where ws = intersect (fst x) (snd x)
              ts = sum $ map (\w -> wins' w s [s!(w-1)]) [i+1..i+(length ws)]
