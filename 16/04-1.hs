import AOC.List (splitOn)
import Data.List (sort, sortBy)

data Room = Room {
    letters :: String,
    sector :: Int,
    checksum :: String
  } deriving Show

main :: IO()
main = interact solve

solve :: String -> String
--solve = show . sum . map sector . filter real . map room . lines
solve = show . sum . map sector . filter real . map room . lines

room :: String -> Room
room xs = Room (concat (init r')) (read (last r')) (init cs)
  where [r, cs] = splitOn '[' xs
        r' = splitOn '-' r

real :: Room -> Bool
real (Room l s c) = sort l' == sort c
  where l' = top 5 l

top :: Int -> String -> [Char]
top n xs = take n $ map snd $ cs
            where cs = sortBy (\(x, y) (z, a) -> compare z x) $ (counts (tail (sort xs)) (1, head (sort xs)))

counts :: String -> (Int, Char) -> [(Int, Char)]
counts [] (y,c) = (y,c):[]
counts (x:xs) (y, c) = if c == x then counts xs (y + 1, c) else (y, c): counts xs (1, x)
