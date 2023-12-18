import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Aoc.PlumbingCombinator

type Position = (Int,Int)
type Part = (Position,Int) -- start position of number

main :: IO()
main = interact solve

solve :: String -> String
solve = show . sum . ratios . fork (gears,parts) . lines

ratios :: ([Position], M.Map Position Part) -> [Int]
ratios ([],_) = []
ratios ((r,c):ps, ms) = if length ps' == 2 then (product ps'):ratios (ps,ms) else ratios (ps,ms)
  where ps' = map snd $ nub $ filter (\p -> snd p >= 0) $ catMaybes $ map (\p -> M.lookup p ms) (adj (r,c))

gears :: [String] -> [Position]
gears xs = foldl(\acc (r,cs) -> acc ++ foldl(\acc' (c,e) -> if e == '*' then (r,c):acc' else acc') [] (zip [0..] cs)) [] (zip [0..] xs)

parts :: [String] -> M.Map Position Part
parts s@(x:xs) = M.fromList s'
  where s' = concatMap (\(r,cs) -> let cs' = zip [0..] cs
                                       ps = parts' r cs' in
                           map (\(c,e) -> ((r,c),mkPart (r,c) e ps)) cs') (zip [0..] s)

parts' :: Int -> [(Int,Char)] -> [Part]
parts' _ [] = []
parts' r ((c,x):xs) = if isDigit x then ((r,c),n):parts' r xs' else parts' r xs
  where n = read $ [x] ++ map snd (takeWhile (isDigit . snd) xs)
        xs' = dropWhile (isDigit . snd) xs


mkPart :: Position -> Char -> [Part] -> Part
mkPart (x,y) c ps
  | isDigit c = findPart (x,y) ps
  | otherwise = ((x,y), -1)

findPart :: Position -> [Part] -> Part
findPart (r,c) ps = fromJust $ find(\((x,y),p) -> let l = length (show p) in
                                       x == r && c >= y && c <= y + l) ps
adj :: Position -> [Position]
adj (r,c) = [(r-1,c),(r-1,c+1),(r,c+1),(r+1,c+1),(r+1,c),(r+1,c-1),(r,c-1),(r-1,c-1)]
