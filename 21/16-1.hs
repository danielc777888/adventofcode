import Data.Char

data Packet
  = Header Version PacketType Packet
  | Literal Int
  | Operator LengthType [Packet]
  deriving (Show)

type Version = Int

type PacketType = Int

type LengthType = Int

type Bits = String

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . versions . fst . mkPacket . toBinary . head . lines

mkPacket :: Bits -> (Packet, Bits)
mkPacket xs = mkHeader xs

mkHeader :: Bits -> (Packet, Bits)
mkHeader xs = (Header (toDecimal v) (toDecimal t) p, ys)
  where
    (v, xs') = splitAt 3 xs
    (t, xs'') = splitAt 3 xs'
    (p, ys) = if (toDecimal t) == 4 then mkLiteral xs'' else mkOperator xs''

mkLiteral :: Bits -> (Packet, Bits)
mkLiteral xs = (Literal (toDecimal b), xs')
  where
    (b, xs') = mkLiteral' xs []

mkLiteral' :: Bits -> Bits -> (Bits, Bits)
mkLiteral' xs ys
  | h == '1' = mkLiteral' xs' (ys ++ x')
  | h == '0' = (ys ++ x', xs')
  | otherwise = error "bummer"
  where
    (x, xs') = splitAt 5 xs
    h = head x
    x' = drop 1 x

mkOperator :: Bits -> (Packet, Bits)
mkOperator xs = if lt == "0" then (Operator (read lt) (mkPackets0 xs0), ys) else (Operator (read lt) ps, ys1)
  where
    (lt, xs') = splitAt 1 xs
    (n, xs'') = if lt == "0" then splitAt 15 xs' else splitAt 11 xs'
    (xs0, ys) = splitAt (toDecimal n) xs''
    (ps, ys1) = mkPackets1 xs'' (toDecimal n) []

mkPackets0 :: Bits -> [Packet]
mkPackets0 xs
  | null xs || all (== '0') xs = []
  | otherwise = p : mkPackets0 xs'
  where
    (p, xs') = mkPacket xs

mkPackets1 :: Bits -> Int -> [Packet] -> ([Packet], Bits)
mkPackets1 xs n ps
  | n == 0 = (ps, xs)
  | otherwise = mkPackets1 xs' (n - 1) (p : ps)
  where
    (p, xs') = mkPacket xs

versions :: Packet -> [Version]
versions (Header v _ p) = v : versions p
versions (Literal _) = []
versions (Operator _ ps) = concatMap versions ps

toDecimal :: Bits -> Int
toDecimal xs = foldr (\(x, y) acc -> acc + (2 ^ x * y)) 0 (zip [l - 1, l - 2 .. 0] (map digitToInt xs))
  where
    l = length xs

toBinary :: String -> Bits
toBinary = concat . foldr f []
  where
    f x xs
      | x == '0' = "0000" : xs
      | x == '1' = "0001" : xs
      | x == '2' = "0010" : xs
      | x == '3' = "0011" : xs
      | x == '4' = "0100" : xs
      | x == '5' = "0101" : xs
      | x == '6' = "0110" : xs
      | x == '7' = "0111" : xs
      | x == '8' = "1000" : xs
      | x == '9' = "1001" : xs
      | x == 'A' = "1010" : xs
      | x == 'B' = "1011" : xs
      | x == 'C' = "1100" : xs
      | x == 'D' = "1101" : xs
      | x == 'E' = "1110" : xs
      | x == 'F' = "1111" : xs
      | otherwise = error ("Unrecognized char :" ++ show x)
