type Cycle = (Int, Int)

data Instruction
  = Noop
  | AddX Int
  deriving (Eq, Show)

main :: IO ()
main = do
  c <- getContents
  print $ part1 c

-- print $ part2 c

part1 :: String -> Int
part1 = sum . map (uncurry strength) . cycles [19, 59 ..] . execute . program . lines

part2 :: String -> String
part2 = id

program :: [String] -> [Instruction]
program xs = map mkInstruction xs

mkInstruction :: String -> Instruction
mkInstruction x = case words x of
  ["noop"] -> Noop
  ["addx", v] -> AddX (read v)
  _ -> error "unrecognized instruction"

execute :: [Instruction] -> [Cycle]
execute xs = execute' xs 1 0

execute' :: [Instruction] -> Int -> Int -> [Cycle]
execute' [] _ _ = []
execute' (x : xs) r c = case x of
  Noop -> (c + 1, r) : execute' xs r (c + 1)
  AddX v -> (c + 1, r) : (c + 2, r) : execute' xs (r + v) (c + 2)

strength :: Int -> Int -> Int
strength x y = x * y

cycles :: [Int] -> [Cycle] -> [Cycle]
cycles is cs = map (cs !!) is'
  where
    is' = takeWhile (<= l) is
    l = length cs
