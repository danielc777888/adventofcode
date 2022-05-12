
main :: IO()
main = interact solve

type Seat = (Int, Int)
type Range  = (Int, Int)

type Half = (Int, Int)

solve :: String -> String
solve = show . maximum . map seatID . map seat . lines

seatID :: Seat -> Int
seatID (x, y) = x * 8 + y

seat :: String -> Seat
seat xs = (r, c)
          where r = row (0, 127) (take 7 xs)
                c = col (0, 7) (drop 7 xs)


row :: Range -> String -> Int
row (l, u) [x] = if x == 'F' then l else u
row (l, u) (x:xs) = if x == 'F' then row (fst hs)  xs else row (snd hs)  xs
         where hs = half (l, u)

col :: Range -> String -> Int
col (l, u) [x] = if x == 'L' then l else u
col (l, u) (x:xs) = if x == 'L' then col (fst hs)  xs else col (snd hs)  xs
         where hs = half (l, u)

half :: (Int, Int) -> (Half, Half)
half (l, u) = (lh, uh)
  where lh = (l, l + floor (fromIntegral (u - l) / 2))
        uh = (l + ceiling (fromIntegral (u - l) / 2), u)
