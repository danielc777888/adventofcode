import Data.List
import Data.Map qualified as M
import Data.Maybe

type Distance = Int

type Route = (Location, Location)

type Location = String

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . maximum . distances . routes . lines

routes :: [String] -> ([Location], M.Map Route Distance)
routes xs = (nub ls, rs)
  where
    xs' = map (readRoute . words) xs
    (ls, rs) = foldr (\(x, y, z) (ls, mr) -> (x : y : ls, M.insert (y, x) z (M.insert (x, y) z mr))) ([], M.empty) xs'

readRoute :: [String] -> (Location, Location, Distance)
readRoute [x, _, y, _, d] = (x, y, read d)

distances :: ([Location], M.Map Route Distance) -> [Distance]
distances (ls, rs) = map (distance rs) (permutations ls)

distance :: M.Map Route Distance -> [Location] -> Distance
distance m [] = 0
distance m (x : []) = 0
distance m (x : y : xs) = fromJust (M.lookup (x, y) m) + distance m (y : xs)
