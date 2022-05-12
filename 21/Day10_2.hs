import Data.List
import Data.Maybe

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . median . sort . map (total . missing) . filter incomplete . lines

incomplete :: [Char] -> Bool
incomplete xs = if isNothing ill then True else False
  where
    ill = illegal (chomps xs)

chomps :: [Char] -> [Char]
chomps xs = if length xs == length xs' then xs else chomps xs'
  where
    xs' = chomp xs

illegal :: [Char] -> Maybe Char
illegal [] = Nothing
illegal (x : []) = Nothing
illegal (x : y : xs)
  | left x && right y = Just y
  | otherwise = illegal (y : xs)

chomp :: [Char] -> [Char]
chomp [] = []
chomp (x : []) = x : []
chomp (x : y : xs)
  | match x y = chomp xs
  | otherwise = x : chomp (y : xs)

left :: Char -> Bool
left c = any (== c) "{[(<"

right :: Char -> Bool
right c = any (== c) "}])>"

match :: Char -> Char -> Bool
match x y
  | x == '{' && y == '}' = True
  | x == '[' && y == ']' = True
  | x == '(' && y == ')' = True
  | x == '<' && y == '>' = True
  | otherwise = False

total :: [Char] -> Int
total = foldl (\acc x -> (acc * 5) + (score x)) 0

score :: Char -> Int
score c
  | c == '(' = 1
  | c == '[' = 2
  | c == '{' = 3
  | c == '<' = 4

missing :: [Char] -> [Char]
missing = reverse . chomps

median :: [Int] -> Int
median xs = xs !! (length xs `div` 2)
