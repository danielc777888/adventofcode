import Data.Maybe

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . map (score . illegal . chomps) . lines

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

score :: Maybe Char -> Int
score Nothing = 0
score (Just c)
  | c == ')' = 3
  | c == ']' = 57
  | c == '}' = 1197
  | c == '>' = 25137
  | otherwise = error ("Unrecognized char: " ++ show c)
