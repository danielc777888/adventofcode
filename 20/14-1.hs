import Data.Char
import Data.List

main :: IO ()
main = interact solve

type Bitmask = (String, [Mem])

type Address = Int

type Mem = (Address, Int)

type Bit = Int

solve :: String -> String
solve = show . sum . map snd . keepLatest . run . program . lines

program :: [String] -> [Bitmask]
program [] = []
program (x : xs)
  | "mask" `isPrefixOf` x = [(bm, ms)] ++ program xs'
  where
    ms = mems xs
    bm = drop 2 $ snd $ break (== '=') x
    xs' = drop (length ms) xs

mems :: [String] -> [(Address, Int)]
mems [] = []
mems (x : xs)
  | "mask" `isPrefixOf` x = []
  | otherwise = [(address, value)] ++ mems xs
  where
    address = read $ takeWhile isDigit $ tail $ snd $ break (== '[') x
    value = read $ drop 2 $ snd $ break (== '=') x

run :: [Bitmask] -> [(Address, Int)]
run [] = []
run (x : xs) = run' x ++ run xs

run' :: Bitmask -> [(Address, Int)]
run' (b, []) = []
run' (b, (m : ms)) = write b m : run' (b, ms)

write :: String -> (Address, Int) -> (Address, Int)
write b (a, v) = (a, v')
  where
    b' = concat $ map show $ reverse $ toBits v
    b'' = (replicate (36 - length b') '0') ++ b'
    z = zip b b''
    m = map (\(x, y) -> if x == 'X' then y else x) z
    v' = toInt $ reverse $ map digitToInt m

toBits :: Int -> [Bit]
toBits 0 = []
toBits n = n `mod` 2 : toBits (n `div` 2)

toInt :: [Bit] -> Int
toInt = foldr (\x y -> x + 2 * y) 0

keepLatest :: [(Address, Int)] -> [(Address, Int)]
keepLatest = foldr (\x y -> if found x y then y else y ++ [x]) []

found :: (Address, Int) -> [(Address, Int)] -> Bool
found _ [] = False
found x (x' : xs) = if fst x == fst x' then True else found x xs
