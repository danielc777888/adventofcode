import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  putStrLn "2024 -- Day 5 -- Print Queue"
  contents <- getContents
  let input = parse contents
  putStrLn ("Part 1: " <> show (part1 input))
  putStrLn ("Part 2: " <> show (part2 input))

-- tuple . (list . list) . tuples
parse :: String -> ([(Int, Int)], [[Int]])
parse s = (rules, pages)
  where
    (sr, sp) = break (=="") $ lines s
    pages = map ints $ drop 1 sp
    rules = map (\l -> let (s1, s2) = break (=='|') l
                in (read s1, read (drop 1 s2))) sr

-- 4281
part1 :: ([(Int, Int)], [[Int]]) -> Int
part1 (rs, ps) = addMiddle ps'
  where rs' = mkRules rs M.empty
        ps' = filter (valid rs') ps

-- 5466
part2 :: ([(Int, Int)], [[Int]]) -> Int
part2 (rs, ps) = addMiddle ps'
  where rs' = mkRules rs M.empty
        ps' = map (correct rs') $ filter (not . valid rs') ps

correct :: M.Map Int (S.Set Int) -> [Int] -> [Int]
correct rs xs = ys
  where rs' = positions rs (length xs) xs xs
        ys = map fst $ sortOn snd rs'

positions :: M.Map Int (S.Set Int) -> Int -> [Int] -> [Int] -> [(Int, Int)]
positions _ _ [] _ = []
positions rs l (x:xs) ys
  | M.member x rs = (x, (l - 1) - (length ss')): positions rs l xs ys
  | otherwise = (x, l - 1):positions rs l xs ys
    where ss = fromJust $ M.lookup x rs
          ys' = ys \\ [x]
          ss' = ys' `intersect` (S.toList ss)

ints :: String -> [Int]
ints [] = []
ints s@(x:xs)
  | x == ',' = ints xs
  | otherwise = read (takeWhile isDigit s) : ints (dropWhile isDigit s)

mkRules :: [(Int, Int)] -> M.Map Int (S.Set Int) -> M.Map Int (S.Set Int)
mkRules [] m = m
mkRules ((x, y):xs) m
  | M.member x m = mkRules xs (M.adjust (S.insert y) x m)
  | otherwise = mkRules xs (M.insert x (S.singleton y) m)

valid :: M.Map Int (S.Set Int) -> [Int] -> Bool
valid _ [] = True
valid _ [_] = True
valid m (x:y:xs) = if asc x y m then valid m (y:xs) else False

asc :: Int -> Int -> M.Map Int (S.Set Int) -> Bool
asc x y m
  | M.member x m = if S.member y ys then True else False
  | otherwise = False
    where ys = fromJust $ M.lookup x m

addMiddle :: [[Int]] -> Int
addMiddle ps = sum $ map (\p ->
    let l = length p in
      if odd l then p !! (l `div` 2) else p !! ((l `div` 2) - 1)) ps


