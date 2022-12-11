import AOC.List (chunk, splitOn)
import AOC.Loop (apply)
import Data.Char
import Data.List
import Prelude hiding (round)

data Monkey = Monkey
  { idx :: Int,
    items :: [Integer],
    op :: [String],
    test :: Integer,
    throwTo :: (Int, Int),
    inspected :: Integer
  }
  deriving (Show)

main :: IO ()
main = do
  c <- getContents
  print $ part1 c

-- print $ part2 c

part1 :: String -> Integer
part1 = business . mostActive 2 . apply 20 round . map mkMonkey . chunk 7 . lines

part2 :: String -> [Integer]
part2 = map inspected . apply 1000 round . map mkMonkey . chunk 7 . lines

mkMonkey :: [String] -> Monkey
mkMonkey (m : is : o : t : it : f : xs) =
  Monkey
    { idx = read $ filter isDigit m,
      items = (map (read . filter isDigit) $ splitOn ',' is) :: [Integer],
      op = words $ drop 2 $ snd $ break (== '=') o,
      test = read $ filter isDigit t,
      throwTo = (read $ filter isDigit it, read $ filter isDigit f),
      inspected = 0
    }

round :: [Monkey] -> [Monkey]
round ms = foldl' (\acc i -> turn acc (acc !! i)) ms (map idx ms)

turn :: [Monkey] -> Monkey -> [Monkey]
turn ms m = snd $ foldl' (\(m', ms') i -> throw ms' m' $ bored $ worry m' i) (m, ms) (items m)

worry :: Monkey -> Integer -> Integer
worry m i = case map (\x -> if x == "old" then show i else x) (op m) of
  [a, "+", b] -> (read a) + (read b)
  [a, "*", b] -> (read a) * (read b)

bored :: Integer -> Integer
bored w = w `div` 3

throw :: [Monkey] -> Monkey -> Integer -> (Monkey, [Monkey])
throw ms tm w = (tm', sortOn idx ms')
  where
    tm' = tm {items = drop 1 (items tm), inspected = (inspected tm) + 1}
    (c1, c2) = throwTo tm
    cm' = if w `mod` (test tm) == 0 then catch (ms !! c1) w else catch (ms !! c2) w
    ms' = unionBy (\m1 m2 -> idx m1 == idx m2) [tm', cm'] ms

catch :: Monkey -> Integer -> Monkey
catch m i = m {items = is}
  where
    is = (items m) ++ [i]

mostActive :: Int -> [Monkey] -> [Monkey]
mostActive x ms = take x $ sortBy (\m1 m2 -> if inspected m2 < inspected m1 then LT else GT) ms

business :: [Monkey] -> Integer
business ms = product (map inspected ms)
