main :: IO()
main = interact solve

type Time = Int
type BusID = Int

solve :: String -> String
solve =  show . mult . earliest . buses . lines

buses :: [String] -> (Time, [BusID])
buses (x:y:xs) = (read x, busIds y)

busIds :: String -> [BusID]
busIds [] = []
busIds xs = if x == "x" then busIds xs' else read x:busIds xs'
       where b = break (==',') xs
             x = fst b
             y = snd b
             xs' = if null y then [] else tail y
                 
                  
earliest :: (Time, [BusID]) -> (Time, Time, BusID)
earliest (t, xs) = (t, fst mi, snd mi)
        where ma = maximum xs
              xs' = map (\id -> head [ (t2, id) | t2 <- [0,id..(t + ma)], t2 >= t]) xs
              mi = minimum xs'

mult :: (Time, Time, BusID) -> Int
mult (t1, t2, b) = (t2 - t1) * b
