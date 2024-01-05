
import Data.List
import qualified Data.Map as M

data Card = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
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

data Hand = Hand {
  handType       :: HandType,
  originalCards  :: [Card],
  strongestCards :: [Card],
  bid            :: Int } deriving Show

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . map (uncurry (*)) . zip [1..] . map bid . sortBy compareHands . map hand . lines

hand :: String -> Hand
hand xs = Hand ht (map card hs) s (read b)
  where [hs,b] = words xs
        (ht, s) = strongest hs

strongest :: String -> (HandType, [Card])
strongest xs
  | M.size hMap == 1 = (FiveOfAKind, map card xs'')
  | M.size hMap == 2 && any (\ys -> length ys == 4) (M.elems hMap) = (FourOfAKind, map card xs'')
  | M.size hMap == 2 && any (\ys -> length ys == 3) (M.elems hMap) = (FullHouse, map card xs'')
  | M.size hMap == 3 && any (\ys -> length ys == 3) (M.elems hMap) = (ThreeOfAKind, map card xs'')
  | M.size hMap == 3 && any (\ys -> length ys == 2) (M.elems hMap) = (TwoPair, map card xs'')
  | M.size hMap == 4 && any (\ys -> length ys == 2) (M.elems hMap) = (OnePair, map card xs'')
  | otherwise = (HighCard, map card xs'')
  where hMap = cardMap xs''
        topc = if xs == "JJJJJ" then 'A'
          else head $ fst $ head $ sortBy (\(_,cs1) (_,cs2) -> compare (length cs2) (length cs1)) $ filter (\(c, _) -> c /= "J") $ M.toList (cardMap xs)
        xs'' = if (any (=='J') xs) then map(\x -> if x == 'J' then topc else x) xs else xs

cardMap :: String -> M.Map String String
cardMap xs = M.fromListWith (++) (zip xs' xs')
  where xs' = map singleton xs

compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2 = if htc == EQ then compare (originalCards h1) (originalCards h2) else htc
  where htc = compare (handType h1) (handType h2)

card :: Char -> Card
card c = case c of
  'J' -> Joker
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight
  '9' -> Nine
  'T' -> Ten
  'Q' -> Queen
  'K' -> King
  'A' -> Ace
  _   -> error ("Unrecognized card pattern " ++ show c)
