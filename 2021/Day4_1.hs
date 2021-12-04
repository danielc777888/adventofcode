
type Row = [(Int, Bool)]
type Board = [Row]
type Game = ([Int], [Board])

main :: IO()
main = interact solve

solve :: String -> String
solve = show . mkGame . lines

mkGame :: [String] -> Game
mkGame ls = (ns', bs)
    where ns = words (map (\c -> if c == ',' then ' ' else c) (head ls))
          ns' = map read ns
          bs = mkBoards (drop 1 ls)

mkBoards :: [String] -> [Board]
mkBoards [] = []
mkBoards xs = rs':mkBoards (dropWhile (\x -> not (null x)) (drop 1 xs))
    where rs = takeWhile (\x -> not (null x)) (drop 1 xs)
          rs' = map(\r -> map (\x -> (read x, False)) (words r)) rs

play :: Game -> ([Int], Board)
play = undefined

score :: ([Int], Board) -> Int
score (ns, b) = last ns * foldr (\(n, m) acc -> if not m then acc + n else 0) 0 (concat b)

wins :: Board -> Bool
wins xss = rw || cw
        where rw = any id (map (\xs -> all id (map snd xs)) xss)
              cols = undefined
              cw = any id (map (\xs -> all id (map snd xs)) cols)

cols :: Board -> Board
cols xss =
