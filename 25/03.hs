import Data.Function
import Data.List

main :: IO ()
main = do
  putStrLn "2025 -- Day 3 -- Lobby"
  contents <- getContents
  let banks = lines contents
  -- putStrLn ("Part 1: " <> show (part1 banks))
  putStrLn ("Part 2: " <> show (part2 banks))

-- 17524
part1 :: [String] -> Int
part1 = sum . map largestJoltage

-- take tail lenth of 12, this in the min
-- take first number look for number larger than it, go left
-- if found replace (not indx of using number)
-- if not found go to next number in final set
part2 :: [String] -> Integer
part2 = sum . map largestJoltage'
--part2 = map largestJoltage'

mss :: Int -> String -> Integer
mss b = read . maximum . filter (short b) . segments

short :: Int -> [a] -> Bool
short b xs = (length xs == b)

segments :: [a] -> [[a]]
segments = concatMap inits . tails

{--
inits, tails :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

tails [] = [[]]
tails (x:xs)  = (x:xs) : tails xs
--}

largestJoltage :: String -> Int
largestJoltage xs = read [d1 ,d2]
  where d1 = maximum (init xs)
        (_, xs') = break (== d1) xs 
        d2 = maximum (tail xs')

largestJoltage' :: String -> Integer
largestJoltage' xs = read mx
  where (ys, zs) = splitAt (length xs - 12) xs
        xs' = zip [0..] xs
        rg = (0, length xs - 12)
        mx = calcLargest xs' rg

calcLargest :: [(Int, Char)] -> (Int, Int) -> String
calcLargest xs (s, e)
  | e == length xs = []
  | otherwise = snd mx : calcLargest xs (s', e')
  where mx = maximumBy (\a b -> case compare (snd a) (snd b) of EQ -> GT; o -> o) xs'
        xs' = filter (\(x, y) -> x >= s && x <= e) xs
        s' = fst mx + 1
        e' = e + 1

findLargest :: String -> String -> String
findLargest [] ys = ys
findLargest _ [] = []
findLargest xs (y:ys) 
  | y > mx  = (y : ys) 
  | y == mx = mx : findLargest xs ys
  | otherwise = mx : findLargest xs' ys
  where mx = maximum xs
        xs' = tail $ dropWhile (\x -> x < mx) xs


findMax :: [(Int, Char)] -> (Int, Char)
findMax = maximumBy (compare `on` snd)

