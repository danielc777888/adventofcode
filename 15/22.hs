-- turn based game, player can select for each turn

import Aoc.PlumbingCombinator (fork)
import Data.List

data Player = Player
  { hitPoints :: Int,
    damage :: Int,
    armour :: Int,
    spent :: Int,
    mana :: Int,
    pSpells :: [Spell]
  }
  deriving (Show)

data Spell = Spell
  { sCost :: Int,
    sEffect :: Int,
    sDamage :: Int,
    sHeal :: Int,
    sArmour :: Int,
    sMana :: Int
  }
  deriving (Show)

data Game = Game
  { p1 :: Player,
    p2 :: Player,
    p1Wins :: Bool
  }
  deriving (Show)

type Purchases = Spell

main :: IO ()
main = undefined

{--
main = interact $ show . fork (solve1, id)

solve1 :: String -> String
solve1 = show . minimum . take 500 . play wins . mkGame

solve2 :: String -> String
solve2 = show . maximum . take 500 . play losses . mkGame

spells :: [Spell]
spells = [magicMissile, drain, shield, poison, recharge]
  where
    magicMissile = Spell 53 1 4 0 0 0
    drain = Spell 73 1 2 2 0 0
    shield = Spell 113 6 0 0 7 0
    poison = Spell 173 6 3 0 0 0
    recharge = Spell 229 5 0 0 0 101

mkBoss :: String -> Player
mkBoss x = Player hp d 0 0 0 []
  where
    ls = lines x
    hp = read $ (words (ls !! 0)) !! 2
    d = read $ (words (ls !! 1)) !! 1

fights :: (Player -> Player -> [Player]) -> Player -> [Int]
fights f boss = map spent (f player boss)
  where
    player = Player 50 0 0 500 0 []

wins :: Game -> Int -> [Game]
wins p b = map snd $ filter fst $ take 500 $ repeat (fight p b True)

losses :: Player -> Player -> [Player]
losses p b = map snd $ filter (not . fst) $ take 500 $ repeat (fight p b True)

p1Turn :: Game -> Game
p1Turn = undefined

p2Turn :: Game -> Game
p2Turn = undefined

nextSpell :: Game -> Game
nextSpell = undefined
--}
