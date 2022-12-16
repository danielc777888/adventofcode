main :: IO ()
main = do
  c <- getContents
  print $ part1 c

-- print $ part2 c

part1 :: String -> String
part1 = id

part2 :: String -> String
part2 = id
