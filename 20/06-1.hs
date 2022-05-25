import Data.List

main :: IO()
main = interact solve

type Answers = [Char]
type Group = [Answers]

solve :: String -> String
solve = show . sum . map counts . groups . lines

groups :: [String] -> [Group]
groups [] = []
groups xs = [x] ++ if null y then [] else groups (tail y)
  where (x, y) = break null xs 

counts :: Group -> Int
counts = length . remDups . sort . filter (/= '\n') .  unlines

remDups :: [Char] -> [Char]
remDups [] = []
remDups (x:xs) = x:remDups (dropWhile (==x) xs) 

