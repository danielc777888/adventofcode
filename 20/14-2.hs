import Control.Monad
import Data.Char
import Data.List

main :: IO ()
main = interact solve

type Bitmask = (String, [Mem])

type Address = Int

type Mem = (Address, Int)

type Bit = Int

solve :: String -> String
solve = show . sum . map snd . keepLatest . sortOn fst . run . program . lines

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
run' (b, (m : ms)) = write b m ++ run' (b, ms)

write :: String -> (Address, Int) -> [(Address, Int)]
write b (a, v) = map (\xs -> (bitsToAddress xs, v)) m'
  where
    b' = addressToBits a
    m' = expand (applyMask b b')

applyMask :: String -> String -> String
applyMask b b' = map modifyBit (zip b b')

modifyBit :: (Char, Char) -> Char
modifyBit (x, y)
  | x == '0' = y
  | x == '1' = '1'
  | x == 'X' = 'X'

expand :: String -> [String]
expand xs = map (\c -> replace c xs) cs
  where
    cs = sort $ replicateM (length $ filter (== 'X') xs) "01"

replace :: String -> String -> String
replace [] ys = ys
replace (x : xs) (y : ys) = if y == 'X' then x : replace xs ys else y : replace (x : xs) ys

addressToBits :: Int -> String
addressToBits n = (replicate (36 - length b) '0') ++ b
  where
    b = concat $ map show $ reverse $ toBits n

bitsToAddress :: String -> Int
bitsToAddress b = toInt $ reverse $ map digitToInt b

toBits :: Int -> [Bit]
toBits 0 = []
toBits n = n `mod` 2 : toBits (n `div` 2)

toInt :: [Bit] -> Int
toInt = foldr (\x y -> x + 2 * y) 0

keepLatest :: [(Address, Int)] -> [(Address, Int)]
keepLatest [] = []
keepLatest (x : xs) = [last (takeWhile (\y -> fst y == fst x) (x : xs))] ++ keepLatest (dropWhile (\y -> fst y == fst x) (x : xs))
