
type Instruction = (String, Int)

main :: IO()
main = interact solve

solve :: String -> String
solve = show . uncurry (*) . execute . map mkInstruction . lines

mkInstruction :: String -> Instruction
mkInstruction s = (ws!!0, read (ws!!1) :: Int)
    where ws = words s

execute :: [Instruction] -> (Int, Int)
execute xs = (h, d)
    where (h, d, _) = foldl step (0, 0, 0) xs

step :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
step (h, d, a) (i, x)
  | i == "forward" = (h+x, d+(a*x), a)
  | i == "down" = (h, d, a+x)
  | i == "up" = (h, d, a-x)
  | otherwise = error ("Unrecognized instruction :" ++ i)

