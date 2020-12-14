main :: IO()
main = interact solve

type Time = Integer
type BusID = Integer

noRestrictions :: Integer
noRestrictions = 1

timeLimit :: Integer
timeLimit = 109000000000000

start :: Integer
start = 99999999999984


solve :: String -> String
solve =  show . earliest . buses . lines

buses :: [String] -> [BusID]
buses (_:y:xs) = busIds y

busIds :: String -> [BusID]
busIds [] = []
busIds xs = if x == "x" then noRestrictions:busIds xs' else read x:busIds xs'
       where b = break (==',') xs
             x = fst b
             y = snd b
             xs' = if null y then [] else tail y
                 
earliest :: [BusID] -> Time
--earliest xs = head [t | t <- [0,(head xs)..timeLimit], match t xs]
earliest xs = head [t-ll | t <- [start, (start + mb)..timeLimit], match xs (t-ll)]
  where mb = maximum xs
        (l, r) = break (==mb) xs
        ll = toInteger (length l)
 
match :: [BusID] -> Time -> Bool
match [] t = True
match (x:xs) t = if t `mod` x == 0 || x == noRestrictions then match xs (t+1) else False


