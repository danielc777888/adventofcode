main :: IO ()
main = interact solve

solve :: String -> String
solve = show . length . filter nice . lines

nice :: String -> Bool
nice xs = if hasRepeats xs && hasPair (prefixes xs) then True else False

pairs :: [(String, String)] -> [String]
pairs xs =
  filter ([] /=) $
    map
      ( \x ->
          let px = fst x
              ys = snd x
           in findLongestPair px (tail ys) (tail px) []
      )
      xs

hasPair :: [(String, String)] -> Bool
hasPair xs = if length (pairs xs) > 0 then True else False

prefixes :: String -> [(String, String)]
prefixes [] = []
prefixes [x] = []
prefixes xs = [(take 2 xs, tail xs)] ++ prefixes (tail xs)

findLongestPair :: String -> String -> String -> [String] -> String
findLongestPair _ [] _ [] = []
findLongestPair px [] _ xs = [head px] ++ last xs
findLongestPair px xs acc ms = if px == (take 2 xs) then findLongestPair px (tail xs) nacc (ms ++ [acc ++ (take 2 xs)]) else findLongestPair px (tail xs) nacc ms
  where
    nacc = acc ++ [head xs]

hasRepeats :: String -> Bool
hasRepeats (x : y : z : xs) = if x == z then True else hasRepeats (y : z : xs)
hasRepeats _ = False
