main :: IO()
main = interact solve


type Layout = [Row]

type Row = [Seat]

data Seat = Floor Position
          | Empty Position
          | Occupied Position deriving (Eq, Show)

type Position = (Int, Int)

solve :: String -> String
solve = show . counts . run . layout (0, 0)  . lines

run :: Layout -> Layout
run xs = run' xs [] 

run' :: Layout -> Layout -> Layout
run' xss yss = if xss == yss then xss else run' (seating xss xss []) xss

seating :: Layout -> Layout -> Layout -> Layout
seating [] xss yss = yss
seating (xs:xss) yss zss = zss ++ [seatingRow xs yss] ++ seating xss yss [] 

seatingRow :: Row -> Layout -> Row
seatingRow [] _ = []
seatingRow (x:xs) xss = [seat x ss] ++ seatingRow xs xss
  where ps = position x
        as = adjacent ps
        ss = seats as xss

seats :: [Position] -> Layout -> [Seat]
seats ps xss = filter(\s -> position s `elem` ps) $ concat xss

seat :: Seat -> [Seat] -> Seat
seat (Empty p) xs = if length (filter occupied xs)  == 0 then Occupied p else Empty p
seat (Occupied p) xs = if length (filter occupied xs) >= 4 then Empty p else Occupied p
seat (Floor p) xs = Floor p
  
adjacent :: Position -> [Position]
adjacent (x, y) = filter (\(x, y) -> x >= 0 && y >= 0) xs
  where xs = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]
 
position :: Seat -> Position
position (Floor p) = p
position (Empty p) = p
position (Occupied p) = p

layout :: Position -> [String] -> Layout
layout _ [] = []
layout (r, c) (x:xs) = [row (r, c) x] ++ layout (r+1, 0) xs

row :: Position -> String -> Row
row _ [] = []
row (r, c) (x:xs)
  | x == '.' = [Floor (r, c)] ++ row (r, c+1) xs
  | x == 'L' = [Empty (r, c)] ++ row (r, c+1) xs
  | x == '#' = [Occupied (r, c)] ++ row (r, c+1) xs
  | otherwise = error "unrecognized seating"

counts :: Layout -> Int
counts = length . filter occupied . concat

occupied :: Seat -> Bool
occupied (Occupied _) = True
occupied _ = False
