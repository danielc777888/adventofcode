import Aoc.PlumbingCombinator (fork)
import Data.List

data Player = Player
  { hitPoints :: Int,
    damage :: Int,
    armour :: Int,
    cost :: Int
  }
  deriving (Show)

data Item = Item
  { icost :: Int,
    idamage :: Int,
    iarmor :: Int
  }
  deriving (Show)

type Purchases = Item

main :: IO ()
main = interact $ show . fork (solve1, solve2)

solve1 :: String -> String
solve1 = show . minimum . fights wins . mkBoss

solve2 :: String -> String
solve2 = show . maximum . fights losses . mkBoss

mkBoss :: String -> Player
mkBoss x = Player hp d a 0
  where
    ls = lines x
    hp = read $ (words (ls !! 0)) !! 2
    d = read $ (words (ls !! 1)) !! 1
    a = read $ (words (ls !! 2)) !! 1

fights :: ([Player] -> Player -> [Player]) -> Player -> [Int]
fights f boss = map cost (f players boss)
  where
    ps = purchases weapons armor rings
    players = map (\(Item c d a) -> Player 100 d a c) ps

wins :: [Player] -> Player -> [Player]
wins [] _ = []
wins (p : ps) boss = if r then w : wins ps boss else wins ps boss
  where
    (r, w) = fight p boss True

losses :: [Player] -> Player -> [Player]
losses [] _ = []
losses (p : ps) boss = if not r then w : losses ps boss else losses ps boss
  where
    (r, w) = fight p boss True

weapons :: [Item]
weapons = [Item 8 4 0, Item 10 5 0, Item 25 6 0, Item 40 7 0, Item 74 8 0]

armor :: [Item]
armor = [Item 0 0 0, Item 13 0 1, Item 31 0 2, Item 53 0 3, Item 75 0 4, Item 102 0 5]

rings :: [Item]
rings = [Item 0 0 0, Item 25 1 0, Item 50 2 0, Item 100 3 0, Item 20 0 1, Item 40 0 2, Item 80 0 3]

purchases :: [Item] -> [Item] -> [Item] -> [Purchases]
purchases w a r = [purchase x y zs | x <- w, y <- a, zs <- zss]
  where
    zss = filter ((<= 2) . length) $ subsequences rings
    purchase x y zs = Item (icost x + icost y + sum (map icost zs)) (idamage x + idamage y + sum (map idamage zs)) (iarmor x + iarmor y + sum (map iarmor zs))

fight :: Player -> Player -> Bool -> (Bool, Player)
fight player boss att
  | hitPoints player <= 0 = (False, player)
  | hitPoints boss <= 0 = (True, player)
  | att = fight player b' False
  | otherwise = fight p' boss True
  where
    b' = attack player boss
    p' = attack boss player

attack :: Player -> Player -> Player
attack (Player hp d a _) (Player hp' d' a' c) = Player (hp' - (d - a')) d' a' c
