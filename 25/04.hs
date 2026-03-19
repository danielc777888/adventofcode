-- TODO: simplify

import Data.Array

type Nat = Int

type Grid = Array (Nat, Nat) Bool

main :: IO ()
main = do
  putStrLn "2025 --- Day 4: Printing Department ---"
  c <- getContents
  let g = mkGrid c 
  putStrLn ("Part 1: " <> show (part1 g))
  putStrLn ("Part 2: " <> show (part2 g))

-- 1505
part1 :: Grid -> Nat
part1 g = foldr (access g) 0 (assocs g)

-- 9182
part2 :: Grid -> Nat 
part2 g = n
  where (_, n, _) = until noMoreRolls removeRolls (g, 0, -1)

mkGrid :: String -> Grid
mkGrid s = array ((0,0), (rowCount - 1, colCount - 1)) [((ri, ci), c == '@') | (ri, r) <- zip [0..] rows, (ci, c) <- zip [0..] r] 
  where rows = lines s
        rowCount = length rows
        colCount = length (head rows)

access :: Grid -> ((Nat, Nat), Bool) -> Int -> Int
access g ((r, c), p) n
  | p == False = n
  | otherwise = if adjacentRolls g (r, c) < 4 then n + 1 else n

access' :: Grid -> ((Nat, Nat), Bool) -> Bool
access' g ((r, c), p) 
  | p == False = False
  | otherwise = if adjacentRolls g (r, c) < 4 then True else False 

adjacentRolls :: Grid -> (Nat, Nat) -> Int
adjacentRolls g (r, c) = length rs
  where is = top <> bottom <> left <> right
        top = [(r-1, c-1), (r-1, c), (r-1, c+1)] 
        bottom = [(r+1, c-1), (r+1, c), (r+1, c+1)]
        left = [(r, c-1)]
        right = [(r, c+1)]
        (maxRow, maxCol) = snd (bounds g)
        is' = filter (\(lr, lc) -> lr >= 0 && lr <= maxRow && lc >= 0 && lc <= maxCol) is
        rs = filter (\i -> g!i) is'

noMoreRolls :: (Grid, Nat, Int) -> Bool
noMoreRolls (_, _, d) = d == 0

removeRolls :: (Grid, Nat, Int) -> (Grid, Nat, Int)
removeRolls (g, n, _) = (g // rrs, n + d, d)
  where rrs = foldr (\x acc -> if access' g x then (fst x, False):acc else acc) [] (assocs g)
        d = length rrs
