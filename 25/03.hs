import Data.List

type Nat = Int

main :: IO ()
main = do
  putStrLn "2025 -- Day 3 -- Lobby"
  contents <- getContents
  let banks = lines contents
  putStrLn ("Part 1: " <> show (part1 banks))
  putStrLn ("Part 2: " <> show (part2 banks))

-- 17445
part1 :: [String] -> Integer
part1 = sum . map (largestJoltage 2)

-- 173229689350551
part2 :: [String] -> Integer
part2 = sum . map (largestJoltage 12)

largestJoltage :: Nat -> String -> Integer
largestJoltage n xs = read mx
  where xs' = zip [0..] xs
        rg = (0, length xs - n)
        mx = calcLargest xs' rg

calcLargest :: [(Nat, Char)] -> (Nat, Nat) -> String
calcLargest xs (s, e)
  | e == length xs = []
  | otherwise = snd mx : calcLargest xs (s', e')
  where mx = maximumBy (\(_, a) (_, b) -> if a == b then GT else compare a b) xs'
        xs' = filter (\(x, _) -> x >= s && x <= e) xs
        s' = fst mx + 1
        e' = e + 1
