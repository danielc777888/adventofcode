import qualified Data.Set as S
import Data.List

type Row = S.Set Int
type Col = S.Set Int
type Board = ([Row], [Col])
type Game = ([Int], [Board])

main :: IO()
main = interact solve

solve :: String -> String
solve = show . score .  play . mkGame . lines

mkGame :: [String] -> Game
mkGame (x:xs) = (ns, bs)
    where ns = map read $ words (map (\c -> if c == ',' then ' ' else c) x)
          bs = mkBoards xs

mkBoards :: [String] -> [Board]
mkBoards [] = []
mkBoards xs = b:mkBoards (dropWhile (\x -> not (null x)) (drop 1 xs))
    where b = mkBoard $ takeWhile (\x -> not (null x)) (drop 1 xs)

mkBoard :: [String] -> Board
mkBoard xs = (rows, cols)
    where ns = map(\x -> map (\y -> (read y)::Int) (words x)) xs
          rows = map S.fromList ns
          cols = map S.fromList $ transpose ns


play :: Game -> (Int, Board)
play ((x:xs), bs) = if null nws then (x, head ws) else play (xs, nws)
    where bs' = map (\(rs, cs) -> ( map(\r -> S.delete x r) rs, map (\c -> S.delete x c) cs)) bs
          ws = filter wins bs'
          nws = filter (not . wins) bs'

score :: (Int, Board) -> Int
score (n, (rs, cs)) = n * sum (concat (map S.toList rs) )

wins :: Board -> Bool
wins (xs, ys) = win xs || win ys
    where win s = any (\x  -> S.null x) s

