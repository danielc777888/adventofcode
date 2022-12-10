-- rope physics
import qualified Data.Set as S
import Data.List

type Motion = (Char, Int)
type Position = (Int, Int)

main :: IO ()
main = do
  c <- getContents
  print $ part1 c

-- print $ part2 c

part1 :: String -> Int
part1 = length . nub . snd . simulate . map mkMotion . lines

part2 :: String -> String
part2 = id

mkMotion :: String -> Motion
mkMotion xs = case words xs of
    [d, n] -> (head d, read n)

simulate :: [Motion] -> ([Position], [Position])
simulate xs = simulate' xs (1,1) (1,1) ([], [])

simulate' :: [Motion] -> Position -> Position -> ([[Position]], [[Position]]) -> ([Position], [Position])
simulate' [] _ _ (hs, ts) = (concat hs, concat ts)
simulate' (x:xs) h t (hs, ts) = simulate' xs (head h') (head t') (h':hs, t':ts)
    where (h', t') = steps h t x

steps :: Position -> Position -> Motion -> ([Position], [Position])
steps h t m@(d, n) = foldl (\(x:xs, y:ys) s -> let (x', y') = step x y m s in
                                    (x':x:xs, y':y:ys)) ([h], [t]) [1..n]

step :: Position -> Position -> Motion -> Int -> (Position, Position)
step h@(hx, hy) t (d, _) s = (h', t')
        where h' = moveHead h d
              t' = moveTail h h' t s

moveTail :: Position -> Position -> Position -> Int -> Position
moveTail h h'@(h'x, h'y) t@(tx, ty) n = if t == h' ||
    (tx + 1 == h'x && ty == h'y) ||
    (tx - 1 == h'x && ty == h'y) ||
    (tx == h'x && ty - 1 == h'y) ||
    (tx == h'x && ty + 1 == h'y) ||
    (tx + 1 == h'x && ty + 1 == h'y) ||
    (tx + 1 == h'x && ty - 1 == h'y) ||
    (tx - 1 == h'x && ty - 1 == h'y) ||
    (tx - 1 == h'x && ty + 1 == h'y)
    then t else h

moveHead :: Position -> Char -> Position
moveHead (hx, hy) d =  case d of
                            'R' -> (hx + 1, hy)
                            'L' -> (hx - 1, hy)
                            'U' -> (hx, hy + 1)
                            'D' -> (hx, hy - 1)