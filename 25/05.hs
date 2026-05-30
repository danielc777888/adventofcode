type Nat = Int
type Range = (Nat, Nat)

main :: IO ()
main = do
  putStrLn "--- Day 5: Cafeteria ---"
  c <- readFile "05.in"
  let i = input c
  putStrLn $ "Part1: " <> show (part1 i)
  putStrLn $ "Part2: " <> show (part2 (fst i))

emptyRange :: Range
emptyRange = (0, 0)

input :: String -> ([Range], [Nat])
input s = (rs', is')
  where ls = lines s
        (rs, is) = break (== "") ls
        rs' = map (\r -> let (i1, i2) = break (== '-') r in
                             (read i1, read (tail i2))) rs
        is' = map read (tail is)

-- 674
part1 :: ([Range], [Nat]) -> Nat
part1 (rs, is) = length (filter (\i -> any (\(i1, i2) -> i >= i1 && i <= i2) rs) is)

-- 352509891817881
part2 :: [Range] -> Nat
part2  = sum . map (\(x, y) -> (y - x) + 1) . mergeRanges

mergeRanges :: [Range] -> [Range]
mergeRanges (x:xs) = foldl (\acc r -> merge r acc) [x] xs 

merge :: Range -> [Range] -> [Range]
merge r [] = if r == emptyRange then [] else [r] 
merge r1 (r2:rs) 
  | r1' == emptyRange = r2':merge r1' rs
  | r2' == emptyRange = merge r1' rs
  | otherwise = r2':merge r1' rs
  where (r1', r2') = removeCommon r1 r2

removeCommon :: Range -> Range -> (Range, Range)
removeCommon r1@(x, y) r2@(x', y')
  | x >= x' && y <= y' = (emptyRange, r2)
  | x < x' && y > y' = (r1, emptyRange)
  | x < x' && y >= x' = ((x, x' - 1), r2)
  | y > y' && x <= y' = ((y' + 1, y), r2)
  | otherwise = (r1, r2)
