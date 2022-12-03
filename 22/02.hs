data Shape
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

instance Ord Shape where
  (<=) Scissors Rock = True
  (<=) Scissors Paper = False
  (<=) Paper Scissors = True
  (<=) Paper Rock = False
  (<=) Rock Paper = True
  (<=) Rock Scissors = False

main :: IO ()
main = do
  c <- getContents
  print $ part1 c
  print $ part2 c

part1 :: String -> Int
part1 =
  sum
    . map
      ( score
          . ( \x -> case words x of
                [a, b] -> (mkShape a, mkShape b)
            )
      )
    . lines

part2 :: String -> Int
part2 =
  sum
    . map
      ( score
          . ( \x -> case words x of
                [a, b] -> (mkShape a, mkNeededShape (mkShape a) b)
            )
      )
    . lines

mkShape :: String -> Shape
mkShape x
  | x == "X" || x == "A" = Rock
  | x == "Y" || x == "B" = Paper
  | otherwise = Scissors

mkNeededShape :: Shape -> String -> Shape
mkNeededShape Rock "X" = Scissors
mkNeededShape Paper "X" = Rock
mkNeededShape Scissors "X" = Paper
mkNeededShape Rock "Z" = Paper
mkNeededShape Paper "Z" = Scissors
mkNeededShape Scissors "Z" = Rock
mkNeededShape s "Y" = s

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

score :: (Shape, Shape) -> Int
score (x, y)
  | y > x = 6 + shapeScore y
  | y < x = shapeScore y
  | otherwise = 3 + shapeScore y
