
import Data.Array

type Map = Array (Int, Int) Char

type Position = (Int, Int)

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
  putStrLn ("Part 1: " <> show (part1 input))
  putStrLn ("Part 2: " <> show (part2 input))

parse :: String -> (Map, Guard)
parse = undefined

--
part1 :: (Map, Guard) ->  Int
part1 = undefined

--
part2 :: (Map, Guard) -> Int
part2 = undefined
