-- D65C3

import Data.Char (intToDigit)

type Instruction = Char
type Instructions = [Instruction]

type Code = [Int]

main :: IO()
main = interact solve

solve :: String -> String
solve = map toChars . run 5 . lines

run :: Int -> [Instructions] -> Code
run x = snd . foldr (\is (y, ys) -> let y' = run' is y in
                         (y', ys ++ [y'])) (x, []) . reverse

run' :: Instructions -> Int -> Int
run' [] c     = c
run' (x:xs) c = run' xs (clamp (move x c))

move :: Instruction -> Int -> Int
move i c
  | i == 'U' && c == 13 = 11
  | i == 'U' = if c `elem` [5,2,1,4,9] then c else c - 4
  | i == 'D' && c == 1 = 3
  | i == 'D' = if c `elem` [5,10,13,12,9] then c else c + 4
  | i == 'L' = if c `elem` [1,2,5,10,13] then c else c - 1
  | i == 'R' = if c `elem` [1,4,9,12,13] then c else c + 1
  | otherwise = error "Invalid instruction"


clamp :: Int -> Int
clamp x
  | x < 1 = 1
  | x > 13 = 13
  | otherwise  = x

toChars :: Int -> Char
toChars x
  | x == 10 = 'A'
  | x == 11 = 'B'
  | x == 12 = 'C'
  | x == 13 = 'D'
  | otherwise = intToDigit x
