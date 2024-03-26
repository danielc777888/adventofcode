-- 84452

type Instruction = Char
type Instructions = [Instruction]

type Code = [Int]

main :: IO()
main = interact solve

solve :: String -> String
solve = show . run 5 . lines

run :: Int -> [Instructions] -> Code
run x = snd . foldr (\is (y, ys) -> let y' = run' is y in
                         (y', ys ++ [y'])) (x, []) . reverse

run' :: Instructions -> Int -> Int
run' [] c     = c
run' (x:xs) c = run' xs (move x c)

move :: Instruction -> Int -> Int
move i c
  | i == 'U' = if c - 3 >= 1 then c - 3 else c
  | i == 'D' = if c + 3 <= 9 then c + 3 else c
  | i == 'L' = if c `elem` [1,4,7] then c else c - 1
  | i == 'R' = if c `elem` [3,6,9]  then c else c + 1
  | otherwise = error "Invalid instruction"
