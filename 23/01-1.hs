import Data.Char

main :: IO()
main = interact solve

solve :: String -> String
solve = show . sum . map (\xs -> let ys = filter isDigit xs
                                 in read [head ys,last ys]) . lines
