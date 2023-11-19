
main :: IO()
main = interact solve

solve :: String -> String
solve = show . sum . map (length . expand) . lines
--solve = show . map expand . lines

expand :: String -> String
expand [] = []
expand ('(':xs) = ex ++ expand xs'
  where (s1,s2) = break (== ')') xs
        (t,r) = marker s1
        ex = concat (replicate r (take t (tail s2)))
        xs' = drop t (tail s2)
expand (x:xs) = x:expand xs

marker :: String -> (Int,Int)
marker xs = case (words (map (\c -> if c == 'x' then ' ' else c) xs)) of
  [x,y] -> (read x, read y)
  _ -> error "marker: Unrecognized pattern"
