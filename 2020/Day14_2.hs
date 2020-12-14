import Data.Char
import Data.List
import Control.Monad

main :: IO()
main = interact solve

type Bitmask = (String, [Mem])
type Address = Int
type Mem = (Address, Int)
type Bit = Int

solve :: String -> String
solve = show. sum . map snd . keepLatest . sortOn fst .  run . program . lines

program :: [String] -> [Bitmask]
program [] = []
program (x:xs)
  | "mask" `isPrefixOf` x = [(bm, ms)] ++ program xs'
  where ms = mems xs
        bm = drop 2 $ snd $ break (=='=') x 
        xs' = drop (length ms) xs

mems :: [String] -> [(Address, Int)]
mems [] = []
mems (x:xs)
  | "mask" `isPrefixOf` x = []
  | otherwise = [(address, value)] ++ mems xs
  where address = read $ takeWhile isDigit $ tail $ snd $ break (=='[') x
        value = read $ drop 2 $ snd $ break (=='=') x

run :: [Bitmask] -> [(Address, Int)]
run []  = []
run (x:xs) = run' x ++ run xs

run' :: Bitmask -> [(Address, Int)]
run' (b, []) = []
run' (b, (m:ms)) = write b m ++ run' (b, ms)

write :: String -> (Address, Int) -> [(Address, Int)]
write b (a, v) = map (\xs -> (toInt (reverse xs), v)) $  map (map digitToInt) m'
  where b' = concat $ map show $ reverse $ toBits a
        b'' = (replicate (36 - length b') '0') ++ b'
        z = zip b b''
        m = map (\(x, y) -> if x == 'X' || x == '1' then x else y) z
        m' = expand m

expand :: String -> [String]
expand xs = map(\c -> replace c xs) cs
   where cs = replicateM (length $ filter (=='X') xs) "01"

replace :: String -> String -> String
replace [] _ = []
replace (x:xs) ys = l ++ [x] ++ replace xs (tail r) 
  where (l, r) = break (=='X') ys
  
toBits :: Int -> [Bit]
toBits 0 = []
toBits n = n `mod` 2 : toBits (n `div` 2)

toInt :: [Bit] -> Int
toInt = foldr (\x y -> x + 2*y) 0

keepLatest :: [(Address, Int)] -> [(Address, Int)]
keepLatest [] = []
keepLatest (x:xs)  = [last (takeWhile (\y -> fst y == fst x) (x:xs))] ++ keepLatest (dropWhile (\y -> fst y == fst x) (x:xs))


