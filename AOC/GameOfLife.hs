module AOC.GameOfLife (
  board,
  corners,
  life,
  alive,
  updatePositions,
  showBoard,
  Board(..),
  Evolve,
  Cell(..) ) where

import qualified Data.Map as M
import Data.Maybe

import AOC.Loop (apply)
import AOC.Number

data Board = Board {
  width :: Nat,
  height :: Nat,
  cells :: M.Map Position Cell } deriving Show

type Position = (Nat, Nat)

type Evolve = (Board -> [Cell] -> (Position, Cell) -> Cell)
              
data Cell = Empty
  | Dead
  | Alive deriving (Eq, Show)

board :: [String] -> Board
board xs = foldr row (Board (length (head xs)) 0 M.empty) (reverse xs)

row :: String -> Board -> Board
row x (Board w h cs) = Board w (h+1) cs'
  where cs' = M.union cs rm
        rm = M.fromList $ zipWith (\a b -> ((a, h), cell b)) [0..] x

showBoard :: Board -> String
showBoard (Board w h xs) = unlines $ map (\y -> map (\x -> showCell $ fromJust (M.lookup (x, y) xs)) [0..w-1]) [0..h-1]

cell :: Char -> Cell
cell x
  | x == '#' = Alive
  | x == '.' = Dead
  | otherwise = Empty

showCell :: Cell -> Char
showCell Alive = '#'
showCell Dead = '.'
showCell Empty = '_'

life :: Evolve -> Nat -> Board -> Board
life f n b = apply n (step f) b

alive :: Cell -> Bool
alive Alive = True
alive _ = False

neighbours :: Board -> Position -> [Cell]
neighbours (Board w h xs) (x, y) = catMaybes [nw, n, ne, e, se, s, sw, w]
  where nw = M.lookup (x-1, y-1) xs
        n = M.lookup (x, y-1) xs
        ne = M.lookup (x+1, y-1) xs
        e = M.lookup (x+1, y) xs
        se = M.lookup (x+1, y+1) xs
        s = M.lookup (x, y+1) xs
        sw = M.lookup (x-1, y+1) xs
        w = M.lookup (x-1, y) xs
        
updatePositions :: Board -> [Position] -> Cell -> Board
updatePositions (Board w h xs) ps c = Board w h xs'
  where xs' = M.union (M.fromList (zip ps (repeat c))) xs

corners :: Nat -> Nat -> [Position]
corners w h = [(0,0), (0, w-1), (h-1, 0), (h-1, w-1)]

step :: Evolve -> Board -> Board
step f b@(Board w h xs) = Board w h xs'
  where xs' = M.mapWithKey (\k v -> let ns = neighbours b k in
                               f b ns (k, v)) xs
