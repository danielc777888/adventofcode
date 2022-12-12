import Data.Char
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S

type HeightMap = M.Map Location Int

type Location = (Int, Int)

type Locations = S.Set Location

main :: IO ()
main = do
  c <- getContents
  print $ part1 c

-- print $ part2 c

part1 :: String -> (HeightMap, (Location, Location))
part1 = heightMap . lines

part2 :: String -> String
part2 = id

heightMap :: [String] -> (HeightMap, (Location, Location))
heightMap xs = (hm, (s, e))
  where
    xs' = zip [0 ..] xs
    hm = M.fromList (concatMap (\(x, r) -> map (\(y, c) -> ((x, y), ord c)) (zip [0 ..] r)) xs')
    s = fst $ head $ filter (\(k, v) -> v == startOrd) $ M.assocs hm
    e = fst $ head $ filter (\(k, v) -> v == endOrd) $ M.assocs hm

paths :: (HeightMap, (Location, Location)) -> [Locations]
paths (hm, (s, e)) = undefined

startOrd :: Int
startOrd = ord 'S'

endOrd :: Int
endOrd = ord 'E'

(fromList [((0, 0), 83), ((0, 1), 97), ((0, 2), 98), ((0, 3), 113), ((0, 4), 112), ((0, 5), 111), ((0, 6), 110), ((0, 7), 109), ((1, 0), 97), ((1, 1), 98), ((1, 2), 99), ((1, 3), 114), ((1, 4), 121), ((1, 5), 120), ((1, 6), 120), ((1, 7), 108), ((2, 0), 97), ((2, 1), 99), ((2, 2), 99), ((2, 3), 115), ((2, 4), 122), ((2, 5), 69), ((2, 6), 120), ((2, 7), 107), ((3, 0), 97), ((3, 1), 99), ((3, 2), 99), ((3, 3), 116), ((3, 4), 117), ((3, 5), 118), ((3, 6), 119), ((3, 7), 106), ((4, 0), 97), ((4, 1), 98), ((4, 2), 100), ((4, 3), 101), ((4, 4), 102), ((4, 5), 103), ((4, 6), 104), ((4, 7), 105)], ((0, 0), (2, 5)))
