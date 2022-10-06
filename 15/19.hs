-- string processing
-- part 1 correct
-- part 2 threw in towel
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Semigroup
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as C
import AOC.PlumbingCombinator (fork)

type Replacements = M.Map C.ByteString [C.ByteString]

main :: IO ()
main = interact $ show . fork (solve1, solve3)

solve1 :: String -> String
solve1 = show . S.size . molecules . replacements . C.lines . C.pack

solve2 :: String -> String
solve2 =  show  . steps . replacements . C.lines . C.pack

replacements :: [C.ByteString] -> (C.ByteString, Replacements)
replacements xs = (mol, reps)
  where mol = head $ reverse xs
        reps = M.fromListWith (++) $ map rep (drop 2 $ reverse xs)
        rep x = case (C.words x) of
          [c, _, r] -> (c, [r])

molecules :: (C.ByteString, Replacements) -> S.Set C.ByteString
molecules x = molecules' x (C.empty, fst x) S.empty

molecules' :: (C.ByteString, Replacements) -> (C.ByteString, C.ByteString) -> S.Set C.ByteString -> S.Set C.ByteString
molecules' (x, xs) (a, b) s
  | C.null b = s
  | M.member c1 xs = molecules' (x, xs) (a' 1, b' 1) (s' c1 1)
  | M.member c2 xs = molecules' (x, xs) (a' 2, b' 2) (s' c2 2)
  | otherwise = molecules' (x, xs) (a' 1, b' 1) s
  where c1 = C.take 1 b
        c2 = C.take 2 b
        a' n = C.append a (C.take n b)
        b' n = C.drop n b
        s' k n = foldl' (\acc r -> S.insert (C.append a (C.append r (C.drop n b))) acc ) s $ fromJust $ M.lookup k xs

steps :: (C.ByteString, Replacements) -> Int
steps (x, xs) = minimum ys
  where xs' = S.fromList $ fromJust $ M.lookup "e" xs
        (ys, s) = steps' (x, C.length x, xs) xs' 1 []

steps' :: (C.ByteString, Int, Replacements) -> S.Set C.ByteString -> Int -> [Int] -> ([Int], S.Set C.ByteString)
steps' (x, l, xs) ys n zs
 | n == 250 = (zs, ys)
 | n < l =  steps' (x, l, xs) ys' (n+1) zs
 | S.member x ys = (n:zs, ys)
 | otherwise = (zs, ys)
 where ys' = S.foldl' (\acc m -> let ms = molecules (m, xs) in
                           S.union acc ms) S.empty (S.take 100 ys)
 
-- credit goes to https://github.com/byorgey/AoC/blob/master/2015/19/19.hs
-- another solution : https://www.reddit.com/r/adventofcode/comments/3xflz8/comment/cy4etju/
solve3 :: String -> String
solve3  = show . moleculeSteps . parseMolecule . tokenize . snd . readInput

data Rule = Rule { ruleFrom :: Element, ruleTo :: Molecule }
  deriving Show

type Rules = M.Map Element [Rule]

type Element = String

data Molecule = Leaf Element
  | RnAr Molecule
  | Cat [Molecule] deriving (Eq, Show)

instance Semigroup Molecule where
  Cat ms1 <> Cat ms2 = Cat (ms1 ++ ms2)
  m1 <> Cat ms2 = Cat (m1 : ms2)
  Cat ms1 <> m2 = Cat (ms1 ++ [m2])
  m1 <> m2 = Cat [m1, m2]

readInput :: String -> (Rules, String)
readInput = first (M.fromListWith (++))
            . (first . map) (\r@(Rule from _) -> (from, [r]))
            . (map readRule *** (!!1))
            . span (not . null)
            . lines

readRule :: String -> Rule
readRule = (Rule <$> (!!0) <*> (parseMolecule . tokenize . (!!2))) . words

tokenize :: String -> [Element]
tokenize = split (startsWithOneOf ['A'..'Z'])

parseMolecule :: [Element] -> Molecule
parseMolecule = fst . pm
  where pm :: [Element] -> (Molecule, [Element])
        pm [] = (Cat [], [])
        pm ("Ar":es) = (Cat [], "Ar":es)
        pm ("Rn":es) = case pm es of
          (m, "Ar":es') -> first (RnAr m <>) (pm es')
        pm (e:es) = first (Leaf e <>) (pm es)
        

moleculeSteps :: Molecule -> Int
moleculeSteps (Leaf e) = 0
moleculeSteps (RnAr m) = moleculeSteps m
moleculeSteps (Cat ms) = sum (map moleculeSteps ms) + sum (map (\m -> length m - 1) ms')
  where ms' = splitOn [Leaf "Y"] ms
