import Data.List

type Point = (Int, Int)

type Segment = [Point]

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . points . segments . lines

segments :: [String] -> [Segment]
segments = map mkSegment
  where
    mkSegment s
      | fst p1 == fst p2 = ey
      | snd p1 == snd p2 = ex
      | otherwise = ed
      where
        ws = words s
        p1 = mkPoint (ws !! 0)
        p2 = mkPoint (ws !! 2)
        ey = [(fst p1, y) | y <- [minimum [snd p1, snd p2] .. maximum [snd p1, snd p2]]]
        ex = [(x, snd p1) | x <- [minimum [fst p1, fst p2] .. maximum [fst p1, fst p2]]]
        ed = zip (expand (fst p1) (fst p2)) (expand (snd p1) (snd p2))
        expand x y = if x > y then [x, x - 1 .. y] else [x .. y]

mkPoint :: String -> Point
mkPoint xs = (x, y)
  where
    x = read (takeWhile (/= ',') xs)
    y = read (tail (dropWhile (/= ',') xs))

points :: [Segment] -> Int
points = length . filter (\(n, _) -> n >= 1) . runCounts 0 . sort . concat

runCounts :: Int -> [Point] -> [(Int, Point)]
runCounts n (x : []) = [(n, x)]
runCounts n (x : y : xs) = if x == y then runCounts (n + 1) (y : xs) else (n, x) : runCounts 0 (y : xs)
