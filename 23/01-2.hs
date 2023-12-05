import Data.Char
import Data.List

main :: IO()
main = interact solve

solve :: String -> String
solve = show . sum . map (\xs -> read [head xs,last xs]) .  map (filter isDigit . replaceWords) . lines

replaceWords :: String -> String
replaceWords [] = []
replaceWords w@(x:xs)
  | "one" `isPrefixOf` w = '1': replaceWords xs
  | "two" `isPrefixOf` w = '2': replaceWords xs
  | "three" `isPrefixOf` w = '3': replaceWords xs
  | "four" `isPrefixOf` w = '4': replaceWords xs
  | "five" `isPrefixOf` w = '5': replaceWords xs
  | "six" `isPrefixOf` w = '6': replaceWords xs
  | "seven" `isPrefixOf` w = '7': replaceWords xs
  | "eight" `isPrefixOf` w = '8': replaceWords xs
  | "nine" `isPrefixOf` w = '9': replaceWords xs
  | otherwise = x: replaceWords xs
