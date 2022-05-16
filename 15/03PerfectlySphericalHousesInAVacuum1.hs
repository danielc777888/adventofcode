main :: IO ()
main = interact (solve . head . lines)

solve :: String -> String
solve = show . length . simulate

data Direction = North | South | East | West

type Location = (Int, Int)

type Locations = [Location]

direction :: Char -> Direction
direction '^' = North
direction 'v' = South
direction '>' = East
direction '<' = West
direction _ = error "invalid direction"

simulate :: String -> Locations
simulate = remdups . qsort . foldr (\x (y : ys) -> move y (direction x) : y : ys) [(0, 0)] . reverse

move :: Location -> Direction -> Location
move (x, y) North = (x, y + 1)
move (x, y) South = (x, y - 1)
move (x, y) East = (x + 1, y)
move (x, y) West = (x - 1, y)

qsort :: Locations -> Locations
qsort [] = []
qsort [x] = [x]
qsort (x : xs) = sortp xs [] []
  where
    sortp [] us vs = qsort us ++ [x] ++ qsort vs
    sortp (y : xs) us vs =
      if fst y < fst x || snd y < snd x
        then sortp xs (y : us) vs
        else sortp xs us (y : vs)

remdups :: Locations -> Locations
remdups [] = []
remdups (x : xs) = x : remdups (dropWhile (== x) xs)
