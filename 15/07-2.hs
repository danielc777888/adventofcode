import Data.Bits hiding (And)
import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = interact solve

ex1 :: String -> String
ex1 _ =
  "123 -> x\n\
  \456 -> y\n\
  \l -> m\n\
  \1 AND y -> n\n\
  \x AND y -> d\n\
  \k LSHIFT 2 -> l\n\
  \x OR y -> e\n\
  \x LSHIFT 2 -> f\n\
  \y RSHIFT 2 -> g\n\
  \h -> j\n\
  \j OR i -> k\n\
  \NOT x -> h\n\
  \NOT y -> i"

solve :: String -> String
solve s = showWires $ sort $ r2
  where
    r1 = run $ sort $ map instruction $ lines s
    a = snd $ fromJust $ find (\w -> fst w == "a") r1
    r2 = run $ sort $ override ("b", a) $ map instruction $ lines s

override :: Wire -> [Instruction] -> [Instruction]
override (x, y) is =
  map
    ( \i ->
        let (i', s) = wire i
         in if i' == x then SignalInstruction (x, y) else i
    )
    is

run :: [Instruction] -> [Wire]
run xs = if all resolved xs then wires else run (updateInstructions xs wires)
  where
    wires = map wire $ filter (\x -> snd (wire x) /= blank) xs

resolved :: Instruction -> Bool
resolved (SignalInstruction _) = True
resolved (GateInstruction _ (_, y)) = y /= blank
resolved (WireInstruction _ (_, y)) = y /= blank

wire :: Instruction -> Wire
wire (SignalInstruction w) = w
wire (GateInstruction _ w) = w
wire (WireInstruction _ w) = w

signalsProvided :: Gate -> Bool
signalsProvided (And (_, x) (_, y)) = x /= blank && y /= blank
signalsProvided (Or (_, x) (_, y)) = x /= blank && y /= blank
signalsProvided (LShift (_, x) _) = x /= blank
signalsProvided (RShift (_, x) _) = x /= blank
signalsProvided (Not (_, x)) = x /= blank

runInstruction :: Instruction -> Instruction
runInstruction (SignalInstruction (x, y)) = SignalInstruction (x, y)
runInstruction (GateInstruction g (x, y)) = GateInstruction g (x, if signalsProvided g then runGate g else y)
runInstruction (WireInstruction (x, y) (x', y')) = WireInstruction (x, y) (x', if not (y == blank) then y else y')

updateInstructions :: [Instruction] -> [Wire] -> [Instruction]
updateInstructions xs ws = map (\i -> if resolved i then i else runInstruction (updateInstruction' i ws)) xs

updateInstruction' :: Instruction -> [Wire] -> Instruction
updateInstruction' i [] = i
updateInstruction' i (x : xs) = if resolved ui then ui else updateInstruction' ui xs
  where
    ui = updateInstruction i x

updateInstruction :: Instruction -> Wire -> Instruction
updateInstruction (SignalInstruction (x, y)) _ = SignalInstruction (x, y)
updateInstruction (GateInstruction (And w1 w2) w3) w4 = GateInstruction (And (updateWire w1 w4) (updateWire w2 w4)) w3
updateInstruction (GateInstruction (Or w1 w2) w3) w4 = GateInstruction (Or (updateWire w1 w4) (updateWire w2 w4)) w3
updateInstruction (GateInstruction (LShift w1 x) w3) w4 = GateInstruction (LShift (updateWire w1 w4) x) w3
updateInstruction (GateInstruction (RShift w1 x) w3) w4 = GateInstruction (RShift (updateWire w1 w4) x) w3
updateInstruction (GateInstruction (Not w1) w3) w4 = GateInstruction (Not (updateWire w1 w4)) w3
updateInstruction (WireInstruction w1 w2) w3 = WireInstruction (updateWire w1 w3) w2

updateWire :: Wire -> Wire -> Wire
updateWire (x, y) (x', y') = if x == x' then (x, y') else (x, y)

instruction :: String -> Instruction
instruction xs
  | "NOT" `isInfixOf` xs = toNot ws
  | "AND" `isInfixOf` xs = toAnd ws
  | "OR" `isInfixOf` xs = toOr ws
  | "LSHIFT" `isInfixOf` xs = toLShift ws
  | "RSHIFT" `isInfixOf` xs = toRShift ws
  | otherwise = if isDigit (xs !! 0) then toSignal ws else toWire ws
  where
    ws = words xs

blank :: Int
blank = (-1)

toNot :: [String] -> Instruction
toNot xs = GateInstruction (Not (xs !! 1, blank)) (xs !! 3, blank) --NOT x -> h

toAnd :: [String] -> Instruction
toAnd xs = GateInstruction (And (x, if x == "1" then 1 else blank) (xs !! 2, blank)) (xs !! 4, blank) --x AND y -> d
  where
    x = xs !! 0

toOr :: [String] -> Instruction
toOr xs = GateInstruction (Or (xs !! 0, blank) (xs !! 2, blank)) (xs !! 4, blank) --x OR y -> d

toLShift :: [String] -> Instruction
toLShift xs = GateInstruction (LShift (xs !! 0, blank) (toInt (xs !! 2))) (xs !! 4, blank) --x LSHIFT 2 -> f

toRShift :: [String] -> Instruction
toRShift xs = GateInstruction (RShift (xs !! 0, blank) (toInt (xs !! 2))) (xs !! 4, blank) --x RSHIFT 2 -> f

toSignal :: [String] -> Instruction
toSignal xs = SignalInstruction (xs !! 2, toInt (xs !! 0)) --123 -> x

toWire :: [String] -> Instruction
toWire xs = WireInstruction (xs !! 0, blank) (xs !! 2, blank) --lx -> x

toInt :: String -> Int
toInt xs = (read xs) :: Int

showWires :: [Wire] -> String
showWires = unlines . map (\x -> (fst x) ++ ":" ++ show (snd x))

runGate :: Gate -> Signal
runGate (And (_, x) (_, y)) = x .&. y
runGate (Or (_, x) (_, y)) = x .|. y
runGate (LShift (_, x) y) = shiftL x y
runGate (RShift (_, x) y) = shiftR x y
runGate (Not (_, x)) = 65535 - x

data Instruction
  = SignalInstruction Wire
  | GateInstruction Gate Wire
  | WireInstruction Wire Wire
  deriving (Eq, Ord, Show)

data Gate
  = And Wire Wire
  | Or Wire Wire
  | LShift Wire Int
  | RShift Wire Int
  | Not Wire
  deriving (Eq, Show, Ord)

type Wire = (Identifier, Signal)

type Signal = Int

type Identifier = String
