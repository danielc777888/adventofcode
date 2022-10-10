import Data.List

main :: IO ()
main = interact solve

type Rule = (Int, Int)

solve :: String -> String
solve = show . sum . invalids . notes . lines

notes :: [String] -> ([Rule], [Int])
notes xs = (rules xs, nearbys xs)

rules :: [String] -> [Rule]
rules [] = []
rules (x : xs)
  | ":" `isInfixOf` x = rule xs' ++ rules xs
  | otherwise = []
  where
    xs' = words (drop 2 (snd (break (== ':') x)))

rule :: [String] -> [Rule]
rule xs = [rule' (xs !! 0)] ++ [rule' (xs !! 2)]

rule' :: String -> Rule
rule' xs = (read l, read (tail r))
  where
    (l, r) = break (== '-') xs

nearbys :: [String] -> [Int]
nearbys [] = []
nearbys (x : xs)
  | "nearby" `isPrefixOf` x = nearby (intercalate "," xs)
  | otherwise = nearbys xs

nearby :: String -> [Int]
nearby [] = []
nearby (x : xs) = read l : if null r then [] else nearby (tail r)
  where
    (l, r) = break (== ',') (x : xs)

invalids :: ([Rule], [Int]) -> [Int]
invalids (xs, ys) = [y | y <- ys, not (valid xs y)]

valid :: [Rule] -> Int -> Bool
valid xs x' = any (\(x, y) -> x' <= y && x' >= x) xs
