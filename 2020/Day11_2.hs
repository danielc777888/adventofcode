import qualified Data.Map as Map
import Data.Maybe

main :: IO()
main = interact solve

type Layout = [Row]

type Row = [Seat]

data Seat = Floor Position
          | Empty Position
          | Occupied Position deriving (Eq, Show)

type Position = (Int, Int)
type SeatMap = Map.Map Position Seat

solve :: String -> String
--solve = show . layout (0, 0)  . lines
solve = show . counts . run . layout (0, 0)  . lines

dims :: Layout -> (Int, Int)
dims xss = (w, h)
    where xs = concat xss
          h = maximum $ map (fst . position) xs
          w = maximum $ map (snd . position) xs

run :: Layout -> (Int, Layout)
run xss = run' xss [] 0 (w, h)
    where xs = concat xss
          h = maximum $ map (fst . position) xs
          w = maximum $ map (snd . position) xs

run' :: Layout -> Layout -> Int -> (Int, Int) -> (Int, Layout)
run' xss yss x d = if xss == yss then (x, xss) else run' (seating xss xss [] mss d) xss (x+1) d
    where mss = Map.fromList [(position s, s) | s <- xs]
          xs = concat xss

seating :: Layout -> Layout -> Layout -> SeatMap -> (Int, Int) -> Layout
seating [] xss yss _ _ = yss
seating (xs:xss) yss zss sm d = zss ++ [seatingRow xs yss sm d] ++ seating xss yss [] sm d

seatingRow :: Row -> Layout -> SeatMap -> (Int, Int) -> Row
seatingRow [] _ _ _ = []
seatingRow (x:xs) xss sm d = (seat x ss) : seatingRow xs xss sm d
  where ps = position x
        as = rays ps d
        ss = seats as sm

--gets all seats, for casted ray
seats :: [[Position]] -> SeatMap -> [Seat]
seats pss sm = foldr(\ps acc -> let v = firstSeat ps sm in
                                if isJust v then (fromJust v):acc else acc) [] pss

firstSeat :: [Position] -> SeatMap -> Maybe Seat
firstSeat [] _ = Nothing
firstSeat (x:xs) sm = if isJust s && isSeat (fromJust s) then s else firstSeat xs sm
        where s = Map.lookup x sm

isSeat :: Seat -> Bool
isSeat (Empty _) = True
isSeat (Occupied _) = True
isSeat (Floor _) = False

seat :: Seat -> [Seat] -> Seat
seat (Empty p) xs = if length (filter occupied xs)  == 0 then Occupied p else Empty p
seat (Occupied p) xs = if length (filter occupied xs) >= 5 then Empty p else Occupied p
seat (Floor p) xs = Floor p

--clockwise from north, split into 8 rays order correctly
rays :: Position -> (Int, Int) -> [[Position]]
rays (x, y) (w, h) = xs --filter (\(x, y) -> x >= 0 && y >= 0) xs
  where xs = [r1,r2,r3,r4,r5,r6,r7,r8]
        r1 = tail $ zip [x,x-1..0] (take (h+1) (repeat y))
        r2 = tail $ zip [x,x-1..0] [y..w]
        r3 = tail $ zip (take ((w+1)-y) (repeat x)) [y..w]
        r4 = tail $ zip [x..h] [y..w]
        r5 = tail $ zip [x..h] (take ((h+1)-x) (repeat y))
        r6 = tail $ zip [x..h] [y,y-1..0]
        r7 = tail $ zip (take (y+1) (repeat x)) [y,y-1..0]
        r8 = tail $ zip [x,x-1..0] [y,y-1..0]

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

counts :: (Int, Layout) -> Int
counts = length . filter occupied . concat . snd

occupied :: Seat -> Bool
occupied (Occupied _) = True
occupied _ = False