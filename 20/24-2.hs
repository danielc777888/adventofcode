import Data.List
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = interact solve

type Grid = Map.Map Coord Bool

type Coord = (Int, Int)

type Directions = [Direction]

type Direction = String

white :: Bool
white = False

black :: Bool
black = True

solve :: String -> String
solve = show . counts . exhibits 100 . renovate . tiles . lines

exhibits :: Int -> Grid -> Grid
exhibits 0 g = g
exhibits n g = exhibits (n - 1) g'
  where
    xs = tilesToFlip g
    g' = Map.union xs g

tiles :: [String] -> (Grid, [Directions])
tiles xs = (g, map directions xs)
  where
    g = initGrid 200

initGrid :: Int -> Grid
initGrid n = Map.fromList [((x, y), False) | x <- [negate n .. n], y <- [negate n .. n], even (x + y)]

directions :: String -> Directions
directions [] = []
directions xs
  | "se" `isPrefixOf` xs = "se" : directions (drop 2 xs)
  | "sw" `isPrefixOf` xs = "sw" : directions (drop 2 xs)
  | "ne" `isPrefixOf` xs = "ne" : directions (drop 2 xs)
  | "nw" `isPrefixOf` xs = "nw" : directions (drop 2 xs)
  | "e" `isPrefixOf` xs = "e" : directions (drop 1 xs)
  | "w" `isPrefixOf` xs = "w" : directions (drop 1 xs)
  | otherwise = error "invalid direction"

renovate :: (Grid, [Directions]) -> Grid
renovate (g, []) = g
renovate (g, x : xs) = renovate (g', xs)
  where
    t = traverseGrid (0, 0) x
    c = fromJust (Map.lookup t g)
    g' = Map.insert t (flipTile c) g

traverseGrid :: Coord -> Directions -> Coord
traverseGrid c [] = c
traverseGrid c (x : xs) = traverseGrid (step c x) xs

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
counts = Map.size . Map.filter (== black)

tilesToFlip :: Grid -> Grid
tilesToFlip g =
  Map.map flipTile $
    Map.filterWithKey
      ( \k v ->
          let a = adjacent k
              n = length $ filter (\c -> fromMaybe white (Map.lookup c g)) a
           in (v == black && (n == 0 || n > 2)) || (v == white && n == 2)
      )
      g

adjacent :: Coord -> [Coord]
adjacent (x, y) = [(x, y - 2), (x, y + 2), (x - 1, y + 1), (x - 1, y - 1), (x + 1, y + 1), (x + 1, y - 1)]
