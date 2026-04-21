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
part1 g = n
  where (_, n, _) = removeRolls (g, 0, -1)

-- 9182
part2 :: Grid -> Nat 
part2 g = n
  where (_, n, _) = until noMoreRolls removeRolls (g, 0, -1)

mkGrid :: String -> Grid
mkGrid s = array ((0,0), (rowCount - 1, colCount - 1)) as
  where rows = lines s
        rowCount = length rows
        colCount = length (head rows)
        idx xs = zip [0..] xs
        as = [((ri, ci), c == '@') | (ri, r) <- idx rows, (ci, c) <- idx r]

access :: Grid -> ((Nat, Nat), Bool) -> Bool
access g ((r, c), p) 
  | p == False = False
  | otherwise = if adjacentRolls g (r, c) < 4 then True else False 

adjacentRolls :: Grid -> (Nat, Nat) -> Int
adjacentRolls g (r, c) = length rs
  where (maxRow, maxCol) = snd (bounds g) 
        is = [(r + x, c + y) | x <- [-1, 0, 1] , y <- [-1, 0, 1], valid (r + x) x (c + y) y]
        rs = filter (\i -> g!i) is
        valid lr x lc y = not (x == 0 && y == 0) && lr >= 0 && lr <= maxRow && lc >= 0 && lc <= maxCol

noMoreRolls :: (Grid, Nat, Int) -> Bool
noMoreRolls (_, _, d) = d == 0

removeRolls :: (Grid, Nat, Int) -> (Grid, Nat, Int)
removeRolls (g, n, _) = (g // rrs, n + d, d)
  where rrs = foldr (\x acc -> if access g x then (fst x, False):acc else acc) [] (assocs g)
        d = length rrs

input :: String
input = "
  ASASA
"
