main :: IO()
main = interact solve

data Step = N Distance
          | S Distance
          | E Distance
          | W Distance
          | F Distance
          | R Degrees
          | L Degrees deriving Show

data Orientation = North Position
               | South Position
               | West Position
               | East Position

type Position  = (Int, Int)
type Distance = Int
type Degrees = Int

solve :: String -> String
solve  = show . distance . run (East (0, 0))  . map step . lines

step :: String -> Step
step (x:xs)
  | x == 'N' = N d
  | x == 'S' = S d
  | x == 'E' = E d
  | x == 'W' = W d
  | x == 'F' = F d
  | x == 'L' = L d
  | x == 'R' = R d
  | otherwise = error "unrecognized step"
  where d = read xs

run :: Orientation -> [Step] -> [Orientation]
run _ [] = []
run o (x:xs) = [o'] ++ run o' xs
  where o' = runStep x o

runStep :: Step -> Orientation -> Orientation
runStep (N d) o = north d o 
runStep (S d) o = south d o
runStep (E d) o = east d o
runStep (W d) o = west d o 
runStep (F d) o = forward d o
runStep (L 90) o = turnLeft o
runStep (R 90) o = turnRight o
runStep (L 180) o = turnAround o
runStep (R 180) o = turnAround o
runStep (L 270) o = turnLeftThreeQuarters o
runStep (R 270) o = turnRightThreeQuarters o
runStep _ _ = error "invalid step"

north :: Distance  -> Orientation -> Orientation
north d (North (x, y)) = North (x + d, y)
north d (South (x, y)) = South (x + d, y)
north d (East (x, y)) = East (x + d, y)
north d (West (x, y)) = West (x + d, y)

south :: Distance  -> Orientation -> Orientation
south d (North (x, y)) = North (x - d, y)
south d (South (x, y)) = South (x - d, y)
south d (East (x, y)) = East (x - d, y)
south d (West (x, y)) = West (x - d, y)

west :: Distance  -> Orientation -> Orientation
west d (North (x, y)) = North (x, y - d)
west d (South (x, y)) = South (x, y - d)
west d (East (x, y)) = East (x, y - d)
west d (West (x, y)) = West (x, y - d)

east :: Distance  -> Orientation -> Orientation
east d (North (x, y)) = North (x, y + d)
east d (South (x, y)) = South (x, y + d)
east d (East (x, y)) = East (x, y + d)
east d (West (x, y)) = West (x, y + d)

forward :: Distance  -> Orientation -> Orientation
forward d (North (x, y)) = North (x + d, y)
forward d (South (x, y)) = South (x - d, y)
forward d (East (x, y)) = East (x, y + d)
forward d (West (x, y)) = West (x, y - d)

turnLeft :: Orientation -> Orientation
turnLeft (North (x, y)) = West (x, y)
turnLeft (West (x, y)) = South (x, y)
turnLeft (South (x, y)) = East (x, y)
turnLeft (East (x, y)) = North (x, y)

turnRight :: Orientation -> Orientation
turnRight (North (x, y)) = East (x, y)
turnRight (West (x, y)) = North (x, y)
turnRight (South (x, y)) = West (x, y)
turnRight (East (x, y)) = South (x, y)

turnLeftThreeQuarters :: Orientation -> Orientation
turnLeftThreeQuarters (North (x, y)) = East (x, y)
turnLeftThreeQuarters (West (x, y)) = North (x, y)
turnLeftThreeQuarters (South (x, y)) = West (x, y)
turnLeftThreeQuarters (East (x, y)) = South (x, y)

turnRightThreeQuarters :: Orientation -> Orientation
turnRightThreeQuarters (North (x, y)) = West (x, y)
turnRightThreeQuarters (West (x, y)) = South (x, y)
turnRightThreeQuarters (South (x, y)) = East (x, y)
turnRightThreeQuarters (East (x, y)) = North (x, y)

turnAround :: Orientation -> Orientation
turnAround (North (x, y)) = South (x, y)
turnAround (West (x, y)) = East (x, y)
turnAround (South (x, y)) = North (x, y)
turnAround (East (x, y)) = West (x, y)

distance :: [Orientation] -> Int
distance xs = abs(x) + abs(y)
  where (x, y)  = position (last xs)
  
position :: Orientation -> Position
position (North p) = p
position (South p) = p
position (West p) = p
position (East p) = p
