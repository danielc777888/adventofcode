import Data.List

type Position = (Int, Int)
type Velocity = (Int, Int)
type Area = (Position, Position)
type Trajectory = [(Position, Velocity)]

main :: IO()
main = interact solve

solve :: String -> String
solve = show . length . nub . filterInitials . hits . simulate . initials (0,0) . target . head . lines

target :: String -> Area
target xs = ((x1, y1), (x2, y2))
    where xs0 = drop 15 xs
          x1 = read (takeWhile (/='.') xs0)
          xs1 = drop 2 (dropWhile (/='.') xs0)
          x2 = read (takeWhile (/=',') xs1)
          xs2 = drop 4 (dropWhile (/=',') xs1)
          y2 = read (takeWhile (/='.') xs2)
          xs3 = drop 2 (dropWhile (/='.') xs2)
          y1 = read xs3

initials :: Position -> Area -> (Position, Area, [Velocity])
initials s@(sx, sy) a@((x1, y1), (x2, y2)) = (s, a, xs)
    where xs = [(x, y) | x <- [sx..x2], y <- [y2..x2]]

simulate :: (Position, Area, [Velocity]) -> (Area, [Trajectory])
simulate (s, a, xs) = (a, map (launch s a) xs)

launch :: Position -> Area -> Velocity -> Trajectory
launch s a v = step a (s, v) [(s, v)]

step :: Area -> (Position, Velocity) -> Trajectory -> Trajectory
step a@((x1, y1), (x2, y2)) t@((px, py), (x, y)) xs
    | px > x2 || py < y2 = xs
    | otherwise = step a (p', v') (t':xs)
    where x' = drag x
          y' = gravity y
          px' = px + x
          py' = py + y
          v' = (x', y')
          p' = (px', py')
          t' = (p' ,v')

gravity :: Int -> Int
gravity y = y - 1

drag :: Int -> Int
drag x
    | x > 0 = x-1
    | x < 0 = x+1
    | otherwise = 0


hits :: (Area, [Trajectory]) -> (Area, [Trajectory])
hits (a, xss) = (a, filter (hit a) xss)

positions :: (Area, [Trajectory]) -> [Position]
positions (_, xss) = concatMap (map fst) xss

filterInitials :: (Area, [Trajectory]) -> [Velocity]
filterInitials (_, xss) = map(\xs -> snd (last xs)) xss

hit :: Area -> Trajectory -> Bool
hit ((x1, y1),(x2, y2)) xs = any(\(x, y) -> x >= x1 && x <= x2 && y <= y1 && y >= y2) (map fst xs)
