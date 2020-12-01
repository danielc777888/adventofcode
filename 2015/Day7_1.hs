import Data.List
import Data.Bits

main :: IO()
main = interact solve

solve :: String -> String
solve = showWires . run . map instruction . lines

run :: [Instruction] -> [Wire]
run = undefined

instruction :: String -> Instruction
instruction xs
  | "NOT" `isInfixOf` xs = toNot ws
  | "AND" `isInfixOf` xs = toAnd ws
  | "OR" `isInfixOf` xs = toOr ws
  | "LSFHIFT" `isInfixOf` xs = toLShift ws
  | "RSHIFT" `isInfixOf` xs = toRShift ws
  | otherwise = toSignal ws
  where ws = words xs


blank :: Int
blank = (-1)

toNot :: [String] -> Instruction
toNot xs = GateInstruction (Not (xs!!1, blank)) (xs!!3, blank) --NOT x -> h

toAnd :: [String] -> Instruction
toAnd xs = GateInstruction (And (xs!!0, blank) (xs!!2, blank)) (xs!!4, blank) --x AND y -> d

toOr :: [String] -> Instruction
toOr xs = GateInstruction (Or (xs!!0, blank) (xs!!2, blank)) (xs!!4, blank) --x OR y -> d

toLShift :: [String] -> Instruction
toLShift xs = GateInstruction (LShift (xs!!0, blank) (toInt (xs!!2))) (xs!!4, blank)  --x LSHIFT 2 -> f

toRShift :: [String] -> Instruction
toRShift xs = GateInstruction (RShift (xs!!0, blank) (toInt (xs!!2))) (xs!!4, blank)  --x RSHIFT 2 -> f
  
toSignal :: [String] -> Instruction
toSignal xs = SignalInstruction ( toInt (xs!!blank))  (xs!!2, toInt (xs!!blank)) --123 -> x


toInt :: String -> Int
toInt xs = (read xs) :: Int

showWires :: [Wire] -> String
showWires = unlines . map (\x -> (fst x) ++ ":" ++ show (snd x))

runGate :: Gate -> Signal
runGate (And (_, x) (_, y)) = x .&. y 
runGate _ = error "cannot run gate"


data Instruction = SignalInstruction Signal Wire 
                 | GateInstruction Gate Wire deriving (Eq, Ord, Show)

data Gate = And Wire Wire
            | Or Wire Wire
            | LShift Wire Int
            | RShift Wire Int
            | Not Wire deriving (Eq, Show, Ord)

type Wire = (Identifier, Signal)
type Signal = Int
type Identifier = String
