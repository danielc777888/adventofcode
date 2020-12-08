import Data.List

main :: IO()
main = interact solve

data Instruction = Nop Int
                 | Acc Int
                 | Jmp Int deriving Show

solve :: String -> String
solve = show . run . map instruction . lines

instruction :: String -> Instruction
instruction xs
  | "nop" `isPrefixOf` xs = Nop n'
  | "acc" `isPrefixOf` xs = Acc n'
  | "jmp" `isPrefixOf` xs = Jmp n'
  | otherwise = error "instruction not recognized."
  where ws = words xs
        s = head (ws!!1)
        n = read (tail (ws!!1)) :: Int
        n' = if s == '-' then negate n else n
          
run :: [Instruction] -> Int
run (x:xs) = run' (x, 0, 0) (zip (x:xs) [0..])  []

run' :: (Instruction, Int, Int) -> [(Instruction, Int)] -> [Int] -> Int
run' ((Nop n), p, a) xs ys = if p `elem` ys then a else run' (next (p+1) a xs) xs (ys++[p])
run' ((Acc n), p, a) xs ys = if p `elem` ys then a else run' (next (p+1) (a+n) xs) xs (ys++[p])
run' ((Jmp n), p, a) xs ys = if p `elem` ys then a else run' (next (p+n) a xs) xs (ys++[p])

next :: Int -> Int -> [(Instruction, Int)] -> (Instruction, Int, Int)
next x y xs = (find' x xs, x, y)

find' :: Int -> [(Instruction, Int)] -> Instruction
find' n xs = head [x | (x, y) <- xs, y == n]
