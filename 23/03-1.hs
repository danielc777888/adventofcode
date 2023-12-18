import Data.Char
import Data.Array
import Data.Maybe
import Data.List
import qualified Data.Set as S

data Elem = ENum Int
  | Symbol Char
  | Period deriving Show

type Engine = Array (Int,Int) Elem

main :: IO()
main = interact solve

solve :: String -> String
solve = show . sum . fst . parts . engine . lines
--solve = show . sort . fst .  parts . engine . lines

engine :: [String] -> Engine
engine s@(x:xs) = array ((0,0),(length s - 1,length x - 1) ) s'
  where s' = concatMap (\(r,cs) -> map (\(c,e) -> ((r,c),mkElem e)) (zip [0..] cs)) (zip [0..] s)

mkElem :: Char -> Elem
mkElem c
  | c == '.' = Period
  | c `elem` symbols = Symbol c
  | isDigit c = ENum (digitToInt c)
  | otherwise = error "Invalid char"

diff :: (S.Set Int, S.Set Int) -> S.Set Int
diff (s1,s2) = S.difference s1 s2

parts :: Engine -> ([Int], [Int])
parts e = foldl (\acc i -> let p = part i e
                               n = fst (fromJust p)
                               isPart = snd (fromJust p)
                               s1 = if isPart then n:(fst acc) else fst acc
                               s2 = if not isPart then n:(snd acc) else snd acc in
                   if isJust p then (s1,s2)  else acc) ([],[]) (indices e)

part :: (Int,Int) -> Engine -> Maybe (Int,Bool)
part (r,c) e = case (e!(r,c)) of
  (ENum n) -> let nn = number (r,c) e
                  p = any isSymb (box (r,c) (fromJust nn) e) in
    if isJust nn then Just (fromJust nn, p) else Nothing
  _ -> Nothing
--  where nn = number n (r,c) e

number :: (Int,Int) -> Engine -> Maybe Int
number (r,c) e
  | c == 0 || not (isNum (e!(r,c-1))) = Just n
  | otherwise = Nothing
  where (_,(u,_)) = bounds e
        c' = min u (c+2)
        n = read $ map (\(ENum en) -> intToDigit en) $ filter isNum $ map(\i -> e!(r,i)) [c..c']

box :: (Int,Int) -> Int -> Engine -> [Elem]
box (r,c) n e
  | n < 10 = map (e!) $ clamp l u $ adj [(r,c)]
  | n >= 10 && n < 100 = map (e!) $ clamp l u $ adj [(r,c),(r,c+1)]
  | otherwise = map (e!) $ clamp l u $ adj [(r,c),(r,c+1),(r,c+2)]
  where ((l,_),(u,_)) = bounds e

adj :: [(Int,Int)] -> [(Int,Int)]
adj xs = nub [(r-1,c),(r-1,c-1),(r,c+1),(r+1,c+1),(r+1,c),(r+1,c-1),(r,c-1),(r-1,c-1),(r-1,c+1),
              (r2-1,c2),(r2-1,c2-1),(r2,c2+1),(r2+1,c2+1),(r2+1,c2),(r2+1,c2-1),(r2,c2-1),(r2-1,c2-1),(r2-1,c2+1)]
  where (r,c) = head xs
        (r2,c2) = last xs

clamp :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
clamp l u xs = filter (\(r,c) -> r >= l && c >= l && r <= u && c <= u) xs

isSymb :: Elem -> Bool
isSymb (Symbol _) = True
isSymb _ = False

isNum :: Elem -> Bool
isNum (ENum _) = True
isNum _ = False

symbols :: [Char]
symbols = ['*','#','+','$','-','%','=','@','&','/']
