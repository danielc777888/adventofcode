
type Instruction = (String, Int)

main :: IO()
main = interact solve

solve :: String -> String
solve = show . uncurry (*) . execute . map mkInstruction . lines

mkInstruction :: String -> Instruction
mkInstruction s = (ws!!0, read (ws!!1) :: Int)
    where ws = words s

execute :: [Instruction] -> (Int, Int)
execute = foldr step (0, 0)

step :: Instruction -> (Int, Int) -> (Int, Int)
step (i, n) (h, d)
  | i == "forward" = (h+n, d)
  | i == "down" = (h, d+n)
  | i == "up" = (h, d-n)
  | otherwise = error ("Unrecognized instruction :" ++ i)


