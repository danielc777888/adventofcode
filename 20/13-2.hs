main :: IO ()
main = interact solve

type Time = Int

type BusID = Int

noRestrictions :: Int
noRestrictions = 1

timeLimit :: Int
-- timeLimit = 200000000000000
-- timeLimit = 101100000000000 --67s old alg
-- timeLimit = 101100000000000 --14 new algo alg and Int
-- timeLimit = 500000000000000
timeLimit = 1000000001000000

start :: Int
-- start = 99999999961000 --multiple of largest num 787
-- start = 299999999993180
start = 0

solve :: String -> String
solve = show . earliest . buses . lines

buses :: [String] -> [BusID]
buses (_ : y : xs) = busIds y

busIds :: String -> [BusID]
busIds [] = []
busIds xs = if x == "x" then noRestrictions : busIds xs' else read x : busIds xs'
  where
    b = break (== ',') xs
    x = fst b
    y = snd b
    xs' = if null y then [] else tail y

earliest :: [BusID] -> Time
earliest xs = head [t - ll | t <- [start, (start + mb) .. timeLimit], match rs (t - ll)]
  where
    mb = maximum xs
    (l, r) = break (== mb) xs
    ll = length l
    rs = runs xs

match :: [(BusID, Int)] -> Time -> Bool
match [(x, y)] t = t `mod` x == 0
match ((x, y) : xs) t = if t `mod` x == 0 then match xs (t + y) else False

runs :: [BusID] -> [(BusID, Int)]
runs [x] = [(x, 0)]
runs (x : xs) = (x, r) : runs (dropWhile (== noRestrictions) xs)
  where
    r = length (takeWhile (== noRestrictions) xs) + 1
