import AOC.List (chunk)
import Data.Char

data Packet
  = PacketInt Int
  | PacketList [Packet]
  deriving (Eq, Show)

main :: IO ()
main = do
  c <- getContents
  print $ part1 c

-- print $ part2 c

part1 :: String -> [(Packet, Packet)]
part1 = map mkPackets . chunk 3 . (++ [" "]) . lines

part2 :: String -> String
part2 = id

mkPackets :: [String] -> (Packet, Packet)
mkPackets [x, y, _] = (mkPacket x, mkPacket y)

mkPacket :: String -> Packet
mkPacket p@(x : xs)
  | x == '[' = PacketList (mkPacket xs : [])
  | x == ']' = mkPacket xs
  | isDigit x = PacketInt (read (takeWhile isDigit p) :: Int)
