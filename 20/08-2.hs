import Data.List

main :: IO()
main = interact solve

data Instruction = Nop Int
                 | Acc Int
                 | Jmp Int deriving Show

solve :: String -> String
solve xs  = show $ head $ dropWhile (\(x, y) -> x == False) $ map run gs
  where ls = lines xs
        is = map instruction ls
        zs = zip is [0..]
        gs = generate zs (positions zs)
        
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
          
run :: [Instruction] -> (Bool, Int)
run (x:xs) = run' (x, 0, 0) (zip (x:xs) [0..])  []

run' :: (Instruction, Int, Int) -> [(Instruction, Int)] -> [Int] -> (Bool, Int)
run' ((Nop n), p, a) xs ys
  | p `elem` ys = (False, a)
  | p + 1 == length xs = (True, a)
  | otherwise = run' (next (p+1) a xs) xs (ys++[p])
run' ((Acc n), p, a) xs ys
  | p `elem` ys = (False,  a)
  | p + 1 == length xs = (True, a + n)
  | otherwise =  run' (next (p+1) (a+n) xs) xs (ys++[p])
run' ((Jmp n), p, a) xs ys
  | p `elem` ys = (False, a)
  | p + n == length xs = (True, a)
  | otherwise = run' (next (p+n) a xs) xs (ys++[p])

next :: Int -> Int -> [(Instruction, Int)] -> (Instruction, Int, Int)
next x y xs = (find' x xs, x, y)

find' :: Int -> [(Instruction, Int)] -> Instruction
find' n xs = head [x | (x, y) <- xs, y == n]

generate :: [(Instruction, Int)] -> [Int] -> [[Instruction]]
generate xs [] = []
generate xs (y:ys) = [[if y'== y then toggle x else x | (x, y') <- xs]] ++ generate xs ys

toggle :: Instruction -> Instruction
toggle (Nop x)  = Jmp x
toggle (Jmp x)  = Nop x
toggle (Acc x)  = Acc x

positions :: [(Instruction, Int)] -> [Int]
positions xs = [y | (x, y) <- xs, fixable x]

fixable :: Instruction -> Bool
fixable (Nop _) = True
fixable (Jmp _) = True
fixable _ = False
