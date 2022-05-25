import Data.List
import Data.Maybe


main :: IO()
main = interact solve

type Rule = (Int, RuleDef)

data RuleDef = Const Char
          | RuleDef [Int]
          | Or [Int] [Int] deriving Show

type Message = String

solve :: String -> String
solve = show . length . filter id . matches . parse . lines
--solve = show . parse . lines

parse :: [String] -> ([Rule], [Message])
parse xs = (rs, ms)
    where bs = break (\s -> s == "") xs
          ms = tail (snd bs)
          rs = rules (fst bs)

rules :: [String] -> [Rule]
rules [] = []
rules (x:xs) = (n, rd):rules xs
    where bs = break (\s -> s == ':') x
          n = read (fst bs) :: Int
          --n = 1
          rd = ruleDef $ tail $ snd bs

ruleDef :: String -> RuleDef
ruleDef xs
    | '|' `elem` xs = Or (ints lh) (ints rh)
    | '"' `elem` xs = Const (xs!!2)
    | otherwise = RuleDef (ints xs)
    where bs = break (\x -> x == '|') xs
          lh = fst bs
          rh = tail $ snd bs

ints :: String -> [Int]
ints xs = map (\x -> read x :: Int) (words xs)
--ints xs = map (\x -> 0) (words xs)

matches :: ([Rule], [Message]) -> [Bool]
matches (rs, ms) = map (match r0 rs) ms
  where r0 = fromJust $ find (\r -> fst r == 0) rs

match :: Rule -> [Rule] -> Message -> Bool
match r rs m = True
