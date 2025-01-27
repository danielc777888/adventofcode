import Data.List
import Data.Maybe
import Data.Char

main :: IO ()
main = do
  putStrLn "2024 -- Day 3 -- Mull It Over"
  contents <- getContents
  let input = parse contents True
  putStrLn ("Part 1: " <> output (part1 input))
  putStrLn ("Part 2: " <> output (part2 input))

parse :: String -> Bool -> [((Int,Int), Bool)]
parse [] _ = []
parse s@(_:xs) d
  | "don't()" `isPrefixOf` s = parse (drop 7 s) False
  | "do()" `isPrefixOf` s = parse (drop 4 s) True
  | "mul(" `isPrefixOf` s = if isJust t then ((fromJust t), d) :parse xs d else parse xs d
  | otherwise = parse xs d
    where t = tuple s

tuple :: String -> Maybe (Int, Int)
tuple xs
  | isNothing x || isNothing y' || length x > 3 || length y' > 3 = Nothing
  | otherwise = Just (read (fromJust x), read (fromJust y'))
  where xs' = drop 4 xs
        (x, ys) = int xs'
        (y, ys') = if "," `isPrefixOf` ys then int (drop 1 ys) else (Nothing, [])
        (y', _) = if ")" `isPrefixOf` ys' then (y, []) else (Nothing, [])

int :: String -> (Maybe String, String)
int xs = if null xs' then (Nothing, xs) else (Just xs', dropWhile isDigit xs)
  where xs' = takeWhile isDigit xs

-- 180233229
part1 :: [((Int, Int), Bool)] -> Int
part1 = sum . map (\((x, y), _) -> x * y)

-- 95411583
part2 :: [((Int, Int), Bool)] -> Int
part2 = sum . map (\((x, y), _) -> x * y) . filter snd

output :: Int -> String
output = show
