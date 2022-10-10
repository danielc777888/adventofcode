main :: IO ()
main = interact solve

data Policy = Policy Range Char Password deriving (Show)

type Range = (Int, Int)

type Password = String

solve :: String -> String
solve = show . length . filter (== True) . map (valid . policy) . lines

policy :: String -> Policy
policy xs = Policy range letter password
  where
    letter = head (ws !! 1)
    password = ws !! 2
    range = (read (fst ys) :: Int, read (drop 1 (snd ys)) :: Int)
    ws = words xs
    ys = break (== '-') (ws !! 0)

valid :: Policy -> Bool
valid (Policy r l p) = count >= fst r && count <= snd r
  where
    count = length $ filter (== l) p
