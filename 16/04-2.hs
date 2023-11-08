import AOC.List (splitOn)
import AOC.Loop (apply)
import Data.List (sort, sortBy, intersperse, isInfixOf)
import Data.Char (ord,chr)

data Room = Room {
    name :: String,
    letters :: String,
    sector :: Int,
    checksum :: String
  } deriving Show

main :: IO()
main = interact solve

solve :: String -> String
solve = show . filter (\(x, y) -> "north" `isInfixOf` y) . map decode . filter real . map room . lines

decode :: Room -> (Int, String)
decode (Room n l s c) = (s, n')
  where n' = apply s shift n

shift :: String -> String
shift [] = []
shift (x:xs)
  | x == '-' = x:shift xs
  | otherwise = chr i':shift xs
    where i = (ord x) + 1
          i' = if i == 123 then 97 else i

room :: String -> Room
room xs = Room r'' (concat (init r')) (read (last r')) (init cs)
  where [r, cs] = splitOn '[' xs
        r' = splitOn '-' r
        r'' = concat $ intersperse " " (init r')

real :: Room -> Bool
real (Room _ l s c) = sort l' == sort c
  where l' = top 5 l

top :: Int -> String -> [Char]
top n xs = take n $ map snd $ cs
            where cs = sortBy (\(x, y) (z, a) -> compare z x) $ (counts (tail (sort xs)) (1, head (sort xs)))

counts :: String -> (Int, Char) -> [(Int, Char)]
counts [] (y,c) = (y,c):[]
counts (x:xs) (y, c) = if c == x then counts xs (y + 1, c) else (y, c): counts xs (1, x)
