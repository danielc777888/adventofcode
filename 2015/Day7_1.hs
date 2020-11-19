import Data.List

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

toNot :: [String] -> Instruction
toNot xs = GateInstruction (Not (xs!!1, 0)) (xs!!3, 0) --NOT x -> h

toAnd :: [String] -> Instruction
toAnd xs = GateInstruction (And (xs!!0, 0) (xs!!2, 0)) (xs!!4, 0) --x AND y -> d

toOr :: [String] -> Instruction
toOr xs = GateInstruction (Or (xs!!0, 0) (xs!!2, 0)) (xs!!4, 0) --x OR y -> d

toLShift :: [String] -> Instruction
toLShift xs = GateInstruction (LShift (xs!!0, 0) (toInt (xs!!2))) (xs!!4, 0)  --x LSHIFT 2 -> f

toRShift :: [String] -> Instruction
toRShift xs = GateInstruction (RShift (xs!!0, 0) (toInt (xs!!2))) (xs!!4, 0)  --x RSHIFT 2 -> f
  
toSignal :: [String] -> Instruction
toSignal xs = SignalInstruction ( toInt (xs!!0))  (xs!!2, toInt (xs!!0)) --123 -> x


toInt :: String -> Int
toInt xs = (read xs) :: Int

showWires :: [Wire] -> String
showWires = undefined

data Instruction = GateInstruction Gate Wire
                 | SignalInstruction Signal Wire deriving (Eq, Show)

data Gate = And Wire Wire
            | Or Wire Wire
            | LShift Wire Int
            | RShift Wire Int
            | Not Wire deriving (Eq, Show)

type Wire = (Identifier, Signal)
type Signal = Int
type Identifier = String
