import Aoc.List (splitOn)

data Cube = Red
  | Green
  | Blue deriving Show

data Game = Game {
  gId :: Int,
  gSubsets :: Subsets} deriving Show

type Subsets = [(Cube,Int)]

main :: IO()
main = interact solve

solve :: String -> String
solve = show . sum .  map (product . minSet . game) . lines

minSet :: Game -> [Int]
minSet (Game _ ss)  = foldr(\(c,n) acc -> case c of
                       Red -> [max n (acc!!0),acc!!1,acc!!2]
                       Blue -> [acc!!0,max n (acc!!1),acc!!2]
                       Green -> [acc!!0,acc!!1,max n (acc!!2)]
                   ) [0,0,0] ss

game :: String -> Game
game xs = Game i (concatMap subset (splitOn ';' b))
  where (g,b) = break (==':') xs
        i = read ((words g)!!1)

subset :: String -> Subsets
subset [] = []
subset xs = map (cube . drop 1) (splitOn ',' xs)
  where cube ys = case (words ys) of
          [n,"blue"] -> (Blue,read n)
          [n,"red"] -> (Red,read n)
          [n,"green"] -> (Green,read n)
          ys' -> error ("Unrecognized cube " ++ concat ys')
