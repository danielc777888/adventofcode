import Data.List
import Data.Array
import qualified Data.Map as M
import Data.Maybe

type Program = Array Int Instruction

type Register = String
type Registers = M.Map String Int

data Instruction = Hlf Register
  | Tpl Register
  | Inc Register
  | Jmp Int
  | Jie Register Int
  | Jio Register Int
  | Nil deriving Show

main :: IO()
main = do
  contents <- getContents
  print $ execute (program contents) 1 (M.fromList [("a", 0), ("b", 0)])
  print $ execute (program contents) 1 (M.fromList [("a", 1), ("b", 0)])

program :: String -> Program
program s = listArray (1, length ls) (map instr ls)
  where ls = lines s
        instr x = case words x of
          ["hlf", r] -> Hlf r
          ["tpl", r] -> Tpl r
          ["inc", r] -> Inc r
          ["jmp", os] -> Jmp (offset os)
          ["jie", r, os] -> Jie (take 1 r) (offset os)
          ["jio", r, os] -> Jio (take 1 r) (offset os)
          _ -> Nil
        offset x
          | "+" `isPrefixOf` x = read (tail x)
          | otherwise = read x
          
execute :: Program -> Int -> Registers -> Registers
execute p i rs
  | i > snd (bounds p) = rs
  | otherwise = execute p i' rs'
  where (i', rs') = execute' (p!i) i rs

execute' :: Instruction -> Int -> Registers -> (Int, Registers)
execute' (Hlf r) i rs = (i+1, M.adjust (`div` 2) r rs)
execute' (Tpl r) i rs = (i+1, M.adjust (* 3) r rs)
execute' (Inc r) i rs = (i+1, M.adjust (+ 1) r rs)
execute' (Jmp o) i rs = (i+o, rs)
execute' (Jie r o) i rs = if maybe False even (M.lookup r rs) then (i+o, rs) else (i+1, rs)
execute' (Jio r o) i rs = if maybe False (== 1) (M.lookup r rs) then (i+o, rs) else (i+1, rs)
execute' _ i rs = (i+1, rs)
