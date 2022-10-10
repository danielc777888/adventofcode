type Game = (Player, Player)

type Player = (Position, Score, Count)

type Position = Int

type Score = Int

type Count = Int

type Dice = Int

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . multiply . play . start . lines

start :: [String] -> Game
start (x : y : xs) = ((pos x, 0, 0), (pos y, 0, 0))
  where
    pos = read . snd . splitAt 28

play :: Game -> Game
play = roll True 1

roll :: Bool -> Dice -> Game -> Game
roll p1r n (p1@(po1, s1, c1), p2@(po2, s2, c2))
  | s1 >= 1000 || s2 >= 1000 = (p1, p2)
  | p1r = roll False n' ((rollPlayer p1 ns), p2)
  | otherwise = roll True n' (p1, (rollPlayer p2 ns))
  where
    ns = map (\n -> if n > 100 then n - 100 else n) [n .. n + 2]
    n' = (last ns) + 1

rollPlayer :: Player -> [Dice] -> Player
rollPlayer (p, s, c) ns = (p'', s + p'', c + length ns)
  where
    p' = p + sum ns
    m = p' `mod` 10
    p'' = if m == 0 then 10 else m

multiply :: Game -> Int
multiply g@((_, _, c1), (_, _, c2)) = ls * (c1 + c2)
  where
    (_, ls, _) = looser g

looser :: Game -> Player
looser (p1@(_, s1, _), p2) = if s1 < 1000 then p1 else p2
