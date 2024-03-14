--
-- turn based game, player can select for each turn

import Data.List

data Player = Player
  { hitPoints :: Int,
    damage    :: Int,
    armour    :: Int,
    spent     :: Int,
    mana      :: Int,
    pSpells   :: [Spell]
  }
  deriving (Show)

data Spell = Spell
  { sCost   :: Int,
    sEffect :: Int,
    sDamage :: Int,
    sHeal   :: Int,
    sArmour :: Int,
    sMana   :: Int
  }
  deriving (Show)

data Game = Game
  { p1     :: Player,
    p2     :: Player,
    p1Wins :: Bool
  }
  deriving (Show)

type Purchases = Spell

main :: IO ()
main = undefined
