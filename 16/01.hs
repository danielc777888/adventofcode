-- taxicab geometry

import qualified Data.Set as S
import Data.Maybe
import Data.List

main :: IO ()
main = do
  contents <- getContents
  print $ (distance origin . fst . follow origin Nothing N S.empty . map mkInstruction . words . filter (not . (==','))) contents
  print $ (distance origin . fromJust . snd . follow origin Nothing N S.empty . map mkInstruction . words . filter (not . (==','))) contents

origin :: (Int, Int)
origin = (0, 0)

data Direction = N | E | S | W deriving Show
data Turn = TurnLeft | TurnRight deriving Show
data Instruction = Instruction Turn Int
  deriving Show

mkInstruction :: String -> Instruction
mkInstruction ('R':xs) = Instruction TurnRight (read xs)
mkInstruction ('L':xs) = Instruction TurnLeft (read xs)
mkInstruction _ = error "invalid instruction"

follow :: (Int, Int) -> Maybe (Int, Int) -> Direction -> S.Set (Int, Int) -> [Instruction] -> ((Int, Int), Maybe (Int, Int))
follow c v d vs [] = (c, v)
follow (x, y) v d vs (i:is) = if isJust v then follow cs v d' vs' is else follow cs v' d' vs' is
  where (d', cs, css) = follow' (x, y) d i
        v' = find (\c -> S.member c vs) css
        vs' = S.union vs (S.fromList css)

follow' :: (Int, Int) -> Direction -> Instruction -> (Direction, (Int, Int), [(Int, Int)])
follow' (x, y) N (Instruction TurnRight n) = (E, (x + n, y), [(g, y) | g <- [x+1..(x+n)]])
follow' (x, y) N (Instruction TurnLeft n) = (W, (x - n, y), [(g, y) | g <- [x-1, x-2..(x-n)]])
follow' (x, y) E (Instruction TurnRight n) = (S, (x, y - n), [(x, g) | g <- [y-1, y-2..(y-n)]])
follow' (x, y) E (Instruction TurnLeft n) = (N, (x, y + n), [(x, g) | g <- [y+1..(y+n)]])
follow' (x, y) S (Instruction TurnRight n) = (W, (x - n, y), [(g, y) | g <- [x-1, x-2..(x-n)]])
follow' (x, y) S (Instruction TurnLeft n) = (E, (x + n, y), [(g, y) | g <- [x+1..(x+n)]])
follow' (x, y) W (Instruction TurnRight n) = (N, (x , y + n), [(x, g) | g <- [y+1..(y+n)]])
follow' (x, y) W (Instruction TurnLeft n) = (S, (x , y - n), [(x, g) | g <- [y-1, y-2..(y-n)]])

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x, y) (x', y') = (abs x' - x) + (abs y' - y)
