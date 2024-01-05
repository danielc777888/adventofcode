
import Data.List
import qualified Data.Map as M

data Card = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace deriving (Eq,Ord,Show)

data HandType = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind deriving (Eq,Ord,Show)

data Hand = Hand HandType [Card] Bid deriving Show

type Bid = Int

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . map (uncurry (*)) . zip [1..] . map(\(Hand _ _ b) -> b) . sortBy compareHands . map hand . lines

hand :: String -> Hand
hand xs = Hand (handType hs) (map card hs) (read b)
  where [hs,b] = words xs

handType :: String -> HandType
handType xs
  | M.size hMap == 1 = FiveOfAKind
  | M.size hMap == 2 && any (\ys -> length ys == 4) (M.elems hMap) = FourOfAKind
  | M.size hMap == 2 && any (\ys -> length ys == 3) (M.elems hMap) = FullHouse
  | M.size hMap == 3 && any (\ys -> length ys == 3) (M.elems hMap) = ThreeOfAKind
  | M.size hMap == 3 && any (\ys -> length ys == 2) (M.elems hMap) = TwoPair
  | M.size hMap == 4 && any (\ys -> length ys == 2) (M.elems hMap) = OnePair
  | otherwise = HighCard
  where hMap = M.fromListWith (++) (zip xs' xs')
        xs' = map singleton xs

compareHands :: Hand -> Hand -> Ordering
compareHands (Hand ht1 cs1 _) (Hand ht2 cs2 _) = if htc == EQ then compare cs1 cs2 else htc
  where htc = compare ht1 ht2

card :: Char -> Card
card c = case c of
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight
  '9' -> Nine
  'T' -> Ten
  'J' -> Jack
  'Q' -> Queen
  'K' -> King
  'A' -> Ace
  _   -> error ("Unrecognized card pattern " ++ show c)
