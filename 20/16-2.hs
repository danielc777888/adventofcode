import Data.List

main :: IO ()
main = interact solve

type Ticket = [Int]

type Positions = [(Position, Field)]

type Field = (Name, [Rule])

type Rule = (Int, Int)

type Name = String

type Position = Int

solve :: String -> String
solve xs = show (product (fieldValues "departure" (remdups ps []) yt))
  where
    ls = lines xs
    fs = fields ls
    yt = yours ls
    nt = nearbys ls
    rs = concat (map snd fs)
    nt' = discard rs nt
    ps = validPositions fs (transpose ([yt] ++ nt'))

fields :: [String] -> [Field]
fields [] = []
fields (x : xs)
  | ":" `isInfixOf` x = [(l, rule xs')] ++ fields xs
  | otherwise = []
  where
    (l, r) = break (== ':') x
    xs' = words (drop 2 r)

rule :: [String] -> [Rule]
rule xs = [rule' (xs !! 0)] ++ [rule' (xs !! 2)]

rule' :: String -> Rule
rule' xs = (read l, read (tail r))
  where
    (l, r) = break (== '-') xs

yours :: [String] -> Ticket
yours [] = []
yours (x : y : xs)
  | "your" `isPrefixOf` x = ticket y
  | otherwise = yours (y : xs)

nearbys :: [String] -> [Ticket]
nearbys [] = []
nearbys (x : xs)
  | "nearby" `isPrefixOf` x = map ticket xs
  | otherwise = nearbys xs

ticket :: String -> Ticket
ticket [] = []
ticket (x : xs) = read l : if null r then [] else ticket (tail r)
  where
    (l, r) = break (== ',') (x : xs)

discard :: [Rule] -> [Ticket] -> [Ticket]
discard rs ts = [t | t <- ts, valid rs t]

valid :: [Rule] -> [Int] -> Bool
valid xs ys = all (\y -> any (\x -> valid' x y) xs) ys

valid' :: Rule -> Int -> Bool
valid' (x, y) x' = x' >= x && x' <= y

validPositions :: [Field] -> [[Int]] -> Positions
validPositions fs ys = [(fst y, f) | y <- ys', f <- fs, valid (snd f) (snd y)]
  where
    ys' = zip [0 ..] ys

remdups :: Positions -> Positions -> Positions
remdups [] ps' = ps'
remdups ps ps' = remdups y (ps' ++ xs)
  where
    xs = filter (\p -> length (filter (\p' -> fst p == fst p') ps) == 1) ps
    ns = map name xs
    (x, y) = partition (\p -> (name p) `elem` ns) ps

name :: (Position, Field) -> Name
name (_, (x, _)) = x

fieldValues :: String -> Positions -> Ticket -> [Int]
fieldValues x ps t = map (\p -> t !! p) ps'
  where
    ps' = map fst $ filter (\(x', y) -> x `isPrefixOf` (fst y)) ps
