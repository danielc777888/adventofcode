import Data.Char
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S

type HeightMap = M.Map Location Int

type Location = (Int, Int)

type Locations = S.Set Location

main :: IO ()
main = do
  c <- getContents
  print $ part1 c

-- print $ part2 c

part1 :: String -> Int
part1 = head . sort . map S.size . take 100 . paths . heightMap . lines

-- part1 = take 10 . paths . heightMap . lines

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
paths (hm, (s, e)) = paths' hm s e S.empty

paths' :: HeightMap -> Location -> Location -> Locations -> [Locations]
paths' hm c e ls
  | c == e = [ls]
  | null ls' = []
  | otherwise = concatMap (\l -> paths' hm l e (S.insert l ls)) ls'
  where
    ls' = validLocations hm c ls

validLocations :: HeightMap -> Location -> Locations -> [Location]
validLocations hm l@(x, y) ls = map fst ls'
  where
    v = fromJust $ M.lookup l hm
    v' = if v == startOrd then ord 'a' else v
    ls' = filter (\((a, b), mv) -> not (S.member (a, b) ls) && isJust mv && ((fromJust mv) - v' == 1 || (fromJust mv) <= v' || (fromJust mv) == endOrd)) $ [nw, w, sw, n, s, ne, e, se]
    nw = ((x - 1, y - 1), M.lookup (x - 1, y - 1) hm)
    w = ((x - 1, y), M.lookup (x - 1, y) hm)
    sw = ((x - 1, y + 1), M.lookup (x - 1, y + 1) hm)
    n = ((x, y - 1), M.lookup (x, y - 1) hm)
    s = ((x, y + 1), M.lookup (x, y + 1) hm)
    ne = ((x + 1, y - 1), M.lookup (x + 1, y - 1) hm)
    e = ((x + 1, y), M.lookup (x + 1, y) hm)
    se = ((x + 1, y + 1), M.lookup (x + 1, y + 1) hm)

startOrd :: Int
startOrd = ord 'S'

endOrd :: Int
endOrd = ord 'E'
