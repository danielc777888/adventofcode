import Data.Maybe
import Data.Array
import Data.List (find, nub)

type Grid = Array (Int, Int) Char -- (row, col)

type Position = (Int, Int) -- (row, col)

data Guard = Guard {
   dir :: Direction,
   pos :: Position,
   poss :: [Position]
  } deriving Show

data Direction = North | East | South | West
  deriving Show

main :: IO ()
main = do
  putStrLn "2024 -- Day 6 -- Guard Gallivant"
  contents <- getContents
  let input = parse contents
  --print input
  putStrLn ("Part 1: " <> show (part1 input))
  putStrLn ("Part 2: " <> show (part2 input))

parse :: String -> (Grid, Guard)
parse = mkGrid
  . concatMap (\(row, line) -> map (\(col, ch) -> ((row, col), ch)) (zip [1..] line))
  . zip [1..] . lines


mkGrid :: [((Int, Int), Char)] -> (Grid, Guard)
mkGrid xs = (array ((1,1), (mx, mx)) xs, Guard { dir = North, pos = gp, poss = [gp]})
  where (gp, _) = fromJust $ find (\(_, c) -> c == '^') xs
        mx = maximum $ map(\((r,_), _) -> r) xs

offGrid :: (Grid, Guard) -> Bool
offGrid (grid, guard) =
  let (r, c) = pos guard
      (_, (r', c')) = bounds grid
  in r < 1 ||r  > r' || c > c' || c < 1

step :: (Grid, Guard) -> (Grid, Guard)
step (grid, guard)
  | offGrid (grid, guard { pos = pos' }) = (grid, guard {pos = pos'})
  | grid!pos' == '#' = (grid, turn guard)
  | otherwise = (grid, guard { pos = pos', poss = poss' })
  where pos' = nextPos (dir guard) (pos guard)
        poss' = pos':(poss guard)

turn :: Guard -> Guard
turn guard = case (dir guard) of
  North -> guard { dir = East }
  East -> guard { dir = South }
  South -> guard { dir = West }
  West -> guard { dir = North }

nextPos :: Direction -> Position -> Position
nextPos d (r, c) = case d of
  North -> (r - 1, c)
  East -> (r, c + 1)
  South -> (r + 1, c)
  West -> (r, c - 1)

        
--
part1 :: (Grid, Guard) ->  Int
part1 grd = let (_, grd') = until offGrid step grd
        in length $ nub $ poss grd'

--
part2 :: (Grid, Guard) -> Int
part2 = undefined
