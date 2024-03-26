-- 106

import Data.List

type Pixel = ((Int,Int),Bool)
type Grid = [[Pixel]]

main :: IO()
main = interact solve

solve :: String -> String
solve = show . length . filter lit . concat . run (grid 50 6) . lines
--solve = showGrid . run (grid 7 3) . lines

grid :: Int -> Int -> Grid
grid w h = map (\r -> map (\c -> ((r,c),False)) cols) rows
  where rows = [0..h-1]
        cols = [0..w-1]

run :: Grid -> [String] -> Grid
run g [] = g
run g (x:xs) = case x' of
  ["rect", w, h]            -> run (rect g (read w) (read h)) xs
  ["rotate","column",x,_,n] -> run (rotateCol g (read x) (read n)) xs
  ["rotate","row",_,y,_,n]  -> run (rotateRow g (read y) (read n)) xs
  _                         -> error "Unrecognized pattern"
  where x' = words (map (\c -> if c == '=' || c == 'x' then ' ' else c) x)

lit :: Pixel -> Bool
lit (_,p) = p

rect :: Grid -> Int -> Int -> Grid
rect g w h = map (\r -> map (\((y,x),p) -> if y < h && x < w then ((y,x),True) else ((y,x),p)) r) g

rotateCol :: Grid -> Int -> Int -> Grid
rotateCol g y n = transpose (rs1 ++ [r'] ++ (drop 1 rs2))
  where g' = transpose g
        w = length (head g')
        (rs1,rs2) = splitAt y g'
        r = head rs2
        r' = sort (map (\(p,px) -> (((fst p + n) `mod` w, snd p),px)) r)

rotateRow :: Grid -> Int -> Int -> Grid
rotateRow g y n = rs1 ++ [r'] ++ (drop 1 rs2)
  where w = length (head g)
        (rs1,rs2) = splitAt y g
        r = head rs2
        r' = sort (map (\(p,px) -> ((fst p, (snd p + n) `mod` w),px)) r)

showGrid :: Grid -> String
showGrid g = unlines $ map (map (\(_,p) -> if p then '#' else '.')) g
