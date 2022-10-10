main :: IO ()
main = interact solve

data Step
  = N Distance
  | S Distance
  | E Distance
  | W Distance
  | F Distance
  | R Degrees
  | L Degrees
  deriving (Show)

type Ship = (Int, Int)

type Waypoint = (Int, Int)

type Distance = Int

type Degrees = Int

solve :: String -> String
solve = show . distance . run ((0, 0), (1, 10)) . map step . lines

step :: String -> Step
step (x : xs)
  | x == 'N' = N d
  | x == 'S' = S d
  | x == 'E' = E d
  | x == 'W' = W d
  | x == 'F' = F d
  | x == 'L' = L d
  | x == 'R' = R d
  | otherwise = error "unrecognized step"
  where
    d = read xs

run :: (Ship, Waypoint) -> [Step] -> [(Ship, Waypoint)]
run _ [] = []
run (s, w) (x : xs) = [(s', w')] ++ run (s', w') xs
  where
    (s', w') = runStep x s w

runStep :: Step -> Ship -> Waypoint -> (Ship, Waypoint)
runStep (N d) s w = (s, north d w)
runStep (S d) s w = (s, south d w)
runStep (E d) s w = (s, east d w)
runStep (W d) s w = (s, west d w)
runStep (F d) s w = (forward d s w, w)
runStep (L 90) s w = (s, turnLeft w)
runStep (R 90) s w = (s, turnRight w)
runStep (L 180) s w = (s, turnAround w)
runStep (R 180) s w = (s, turnAround w)
runStep (L 270) s w = (s, turnRight w)
runStep (R 270) s w = (s, turnLeft w)
runStep _ _ _ = error "invalid step"

north :: Distance -> Waypoint -> Waypoint
north d (x, y) = (x + d, y)

south :: Distance -> Waypoint -> Waypoint
south d (x, y) = (x - d, y)

west :: Distance -> Waypoint -> Waypoint
west d (x, y) = (x, y - d)

east :: Distance -> Waypoint -> Waypoint
east d (x, y) = (x, y + d)

forward :: Int -> Ship -> Waypoint -> Ship
forward n (x, y) (x', y') = (x + (n * x'), y + (n * y'))

turnLeft :: Waypoint -> Waypoint
turnLeft (x, y) = (y, negate x)

turnRight :: Waypoint -> Waypoint
turnRight (x, y) = (negate y, x)

turnAround :: Waypoint -> Waypoint
turnAround (x, y) = (negate x, negate y)

distance :: [(Ship, Waypoint)] -> Int
distance xs = abs (x) + abs (y)
  where
    ((x, y), (x', y')) = last xs
