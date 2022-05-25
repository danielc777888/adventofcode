
type Game = (Player, Player)
type Player = (Position, Score, Count)
type Position = Int
type Score = Int
type Count = Int
type Dice = Int

main :: IO()
main = interact solve

solve :: String -> String
solve = show . play. start . lines

start :: [String] -> Game
start (x:y:xs) = ((pos x, 0, 0), (pos y, 0, 0))
    where pos = read . snd . splitAt 28

play :: Game -> (Int, Int)
play = roll True 1 (0, 0)

roll :: Bool -> Dice -> (Int, Int) -> Game -> (Int, Int)
roll p1r n (w1, w2) (p1@(po1, s1, c1), p2@(po2, s2, c2))
    | s1 >= 21  = (1+w1,w2)
    | s2 >= 21  = (w1,w2+1)
    | p1r = sumWins p1b
    | otherwise = sumWins p2b
    where n' = if n > 3 then n - 3 else n
          n1 = n' + 1
          n2 = n' + 2
          n3 = n' + 3
          p1b = [roll False n1 (w1, w2) ((rollPlayer p1 n'), p2), roll False n2 (w1, w2) ((rollPlayer p1 n'), p2), roll False n3 (w1, w2) ((rollPlayer p1 n'), p2)]
          p2b = [roll True n1 (w1, w2) (p1, (rollPlayer p2 n')), roll True n2 (w1, w2) (p1, (rollPlayer p2 n')), roll True n3 (w1, w2) (p1, (rollPlayer p2 n'))]
          sumWins ws = (w1 + sum (map fst ws), w2 + sum (map snd ws))

rollPlayer :: Player -> Dice -> Player
rollPlayer (p, s, c) n = (p'', s + p'', c + 1)
    where
        p' = p + n
        m = p' `mod` 10
        p'' = if m == 0 then 10 else m

