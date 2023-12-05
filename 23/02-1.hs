import Aoc.List (splitOn)

data Cube = Red
  | Green
  | Blue deriving (Show,Eq)

data Game = Game {
  gId :: Int,
  gSubsets :: [Subset]} deriving Show

type Subset = [(Cube,Int)]

main :: IO()
main = interact solve

solve :: String -> String
solve = show . sum . map gId . filter (possible bag) . map game . lines
--solve = show . map game . lines

bag :: Subset
bag = [(Red,12),(Green,13),(Blue,14)]

game :: String -> Game
game xs = Game i (map subset (splitOn ';' b))
  where (g,b) = break (==':') xs
        i = read ((words g)!!1)

subset :: String -> Subset
subset [] = []
subset xs = map (cube . drop 1) (splitOn ',' xs)
  where cube ys = case (words ys) of
          [n,"blue"] -> (Blue,read n)
          [n,"red"] -> (Red,read n)
          [n,"green"] -> (Green,read n)
          ys -> error ("Unrecognized cube " ++ concat ys)

possible :: Subset -> Game -> Bool
possible ss (Game _ gs) = null ss'
  where gs' = concat gs
        ss' = filter(\(c,n) -> any (\(c',n') -> c' == c && n' > n) gs') ss
