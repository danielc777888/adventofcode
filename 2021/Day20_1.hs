import qualified Data.Map as M
import Data.Array

type Image = M.Map Point Pixel
type Algo = Array Int Pixel
type Pixel = Char
type Point = (Int, Int)

main :: IO()
main = interact solve

solve :: String -> String
solve = show . inputs . lines

inputs :: [String] -> (Algo, Image)
inputs xs = (a, i)
    where a = listArray (0,length x - 1) x
          x = head xs
          i = mkImage (drop 2 xs)

mkImage :: [String] -> Image
mkImage xs =
    where padLength = length (head xs)
          padRow = take padLength (repeat '.')
          padCol = take 2 (repeat '.')
          xs' = map (\x -> (padCol:x) ++ [padCol]) xs

enhance :: Int -> (Algo, Image) -> Image
enhance n = undefined

countLit :: Image -> Int
countLit = undefined
