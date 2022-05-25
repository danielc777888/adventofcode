import Data.List
import Data.Maybe
import qualified Data.Map as Map

main :: IO()
main = interact solve

type Grid = Map.Map Coord Bool

type Coord = (Int, Int)
type Directions = [Direction]
type Direction = String

solve :: String -> String
solve = show . counts . flippin . tiles . lines

tiles :: [String] -> (Grid, [Directions])
tiles xs = (g, map directions xs)
  where g = initGrid 50

initGrid :: Int -> Grid
initGrid n = Map.fromList [((x, y), False) | x <- [negate n..n], y <- [negate n..n], even (x + y)]

directions :: String -> Directions
directions [] = []
directions xs
  | "se" `isPrefixOf` xs = "se":directions (drop 2 xs)
  | "sw" `isPrefixOf` xs = "sw":directions (drop 2 xs)
  | "ne" `isPrefixOf` xs = "ne":directions (drop 2 xs)
  | "nw" `isPrefixOf` xs = "nw":directions (drop 2 xs)
  | "e" `isPrefixOf` xs = "e":directions (drop 1 xs)
  | "w" `isPrefixOf` xs = "w":directions (drop 1 xs)
  | otherwise = error "invalid direction"
  
flippin :: (Grid, [Directions]) -> Grid
flippin (g, []) = g
flippin (g, x:xs) = flippin (g', xs)
  where t = traverseGrid (0,0) x
        c = fromJust (Map.lookup t g)
        g' = Map.insert t (flipTile c) g

traverseGrid :: Coord -> Directions -> Coord
traverseGrid c [] = c
traverseGrid c (x:xs) = traverseGrid (step c x)  xs

flipTile :: Bool -> Bool
flipTile True = False
flipTile False = True

step :: Coord -> Direction -> Coord
step (x, y) d
  | d == "e" = (x, y + 2)
  | d == "w" = (x, y - 2)
  | d == "se" = (x - 1, y + 1)
  | d == "sw" = (x - 1, y - 1)
  | d == "ne" = (x + 1, y + 1)
  | d == "nw" = (x + 1, y - 1)
  | otherwise = error "invalid move"

counts :: Grid -> Int
counts = Map.size . Map.filter black

black :: Bool -> Bool
black True = True
black False = False
