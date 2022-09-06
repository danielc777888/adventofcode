import Data.List
import Data.Function
import AOC.Loop (apply)
import Data.Tuple

type Distance = Int
type Time = Int
type Fly = (Distance, Time)
type Rest = (Distance, Time)
type Points = Int

data Reindeer = Reindeer {
  name :: String,
  actions :: (Fly, Rest),
  counter :: (Distance, Time),
  distance :: Distance,
  points :: Points } deriving Show

main :: IO()
main = interact solve'

solve :: String -> String
solve = show . winner distance . race 2503 . map reindeer . lines 

solve' :: String -> String
solve' = show . winner points . race 2503 . map reindeer . lines

reindeer :: String -> Reindeer
reindeer xs = case words xs of
                [n, _, _, sp, _, _, st, _, _, _, _, _, _, r, _] -> Reindeer n (fly, rest) fly 0 0
                  where fly = (read sp, read st)
                        rest = (0, read r)

race :: Int -> [Reindeer] -> [Reindeer]
race t xs = apply t (tally . map step) xs

winner :: (Reindeer -> Int) -> [Reindeer] -> Reindeer
winner f = maximumBy (compare `on` f)

step :: Reindeer -> Reindeer
step (Reindeer n as (d, t) td p)
 | t == 1 = Reindeer n as' (fst as') (td + d') p
 | otherwise = Reindeer n as (d, t - 1) (td + d) p
 where as' = swap as
       d' = fst $ fst as

tally :: [Reindeer] -> [Reindeer]
tally xs = map (\r -> if distance r == md then r { points = (points r) + 1 } else r) xs
  where md = maximum (map distance xs)
