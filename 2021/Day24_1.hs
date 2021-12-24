import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as M

data Instruction = Inp Operand
    | Add Operand Operand
    | Mul Operand Operand
    | Div Operand Operand
    | Mod Operand Operand
    | Eql Operand Operand
    deriving Show

data Operand = Var Char
    | Number Int
    deriving Show

type VMap = M.Map Char Int

main :: IO()
main = interact solve

solve :: String -> String
solve = show . maximum . checks (99999999111111, 99999999999999) . instructions . lines
--solve = show . checks (13579246899995,13579246899999) . instructions . lines

checks :: (Int, Int) -> [Instruction] -> [Int]
checks (x, y) xs
  | x > y = []
  | zero  = checks (x', y) xs
  | otherwise = if valid then x:checks (x+1, y) xs else checks (x+1, y) xs
    where ys = map digitToInt (show x)
          zero = 0 `elem` ys
          x' = read $ concatMap show (map (\i -> if i == 0 then 1 else i) ys)
          valid = check ys xs mkVMap

mkVMap :: VMap
mkVMap = M.fromList [('x',0),('y',0),('z',0),('w',0)]

check :: [Int] -> [Instruction] -> VMap -> Bool
check [] [] vm = fromJust (M.lookup 'z' vm) == 0
check xs (y:ys) vm = if h then False else check xs' ys vm'
    where (xs', vm', h) = execute y xs vm

check2 :: [Int] -> [Instruction] -> VMap -> ([Int], VMap)
check2 [] [] vm = ([], vm)
check2 xs (y:ys) vm = if h then (xs', vm') else check2 xs' ys vm'
    where (xs', vm', h) = execute y xs vm


execute :: Instruction -> [Int] -> VMap -> ([Int], VMap, Bool)
execute (Inp (Var v)) (x:[]) vm = ([], M.insert v x vm, False)
execute (Inp (Var v)) (x:xs) vm = (xs, M.insert v x vm, False)
execute (Add (Var v) (Number i)) xs vm = (xs, M.insert v (i + num v vm) vm, False)
execute (Add (Var v) (Var v2)) xs vm = (xs, M.insert v (num v vm + num v2 vm) vm, False)
execute (Mul (Var v) (Number i)) xs vm = (xs, M.insert v (i * num v vm) vm, False)
execute (Mul (Var v) (Var v2)) xs vm = (xs, M.insert v (num v vm * num v2 vm) vm, False)
execute (Div (Var v) (Number i)) xs vm = if i == 0 then (xs, vm, True) else (xs, M.insert v (num v vm `div` i) vm, False)
execute (Div (Var v) (Var v2)) xs vm = if num v2 vm == 0 then (xs, vm, True) else (xs, M.insert v (num v vm `div` num v2 vm) vm, False)
execute (Mod (Var v) (Number i)) xs vm = if num v vm < 0 || i <= 0 then (xs, vm, True) else (xs, M.insert v (num v vm `mod` i) vm, False)
execute (Mod (Var v) (Var v2)) xs vm = if num v vm < 0 || num v2 vm <= 0 then (xs, vm, True) else (xs, M.insert v (num v vm `mod` num v2 vm) vm, False)
execute (Eql (Var v) (Number i)) xs vm = (xs, M.insert v (if i == num v vm then 1 else 0) vm, False)
execute (Eql (Var v) (Var v2)) xs vm = (xs, M.insert v (if num v vm == num v2 vm then 1 else 0) vm, False)
execute _ _ _ = error "invalid params for execute"


num :: Char -> VMap -> Int
num c vm = fromJust (M.lookup c vm)

instructions :: [String] -> [Instruction]
instructions [] = []
instructions (x:xs) = instruction x: instructions xs

instruction :: String -> Instruction
instruction x
  | "inp" `isPrefixOf` x = Inp (Var v)
  | "add" `isPrefixOf` x = if isLetter (head v2) then Add (Var v) (Var (head v2)) else Add (Var v) (Number n)
  | "mul" `isPrefixOf` x = if isLetter (head v2) then Mul (Var v) (Var (head v2)) else Mul (Var v) (Number n)
  | "div" `isPrefixOf` x = if isLetter (head v2) then Div (Var v) (Var (head v2)) else Div (Var v) (Number n)
  | "mod" `isPrefixOf` x = if isLetter (head v2) then Mod (Var v) (Var (head v2)) else Mod (Var v) (Number n)
  | "eql" `isPrefixOf` x = if isLetter (head v2) then Eql (Var v) (Var (head v2)) else Eql (Var v) (Number n)
  | otherwise = error ("invalid instruction: " ++ x)
      where ws = words x
            v = head $ ws!!1
            v2 = ws!!2
            n = read v2
