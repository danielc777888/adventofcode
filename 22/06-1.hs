main :: IO ()
main = interact solve

solve :: String -> String
solve = show . countLit . foldr execute grid . map createInstruction . reverse . lines

data State = On | Off deriving (Show, Eq)

type X = Int

type Y = Int

type Pair = (X, Y)

type Light = (Pair, State)

type Row = [Light]

type Grid = [Row]

data Rectangle = Rectangle Pair Pair deriving (Show)

data Instruction
  = TurnOn Rectangle
  | TurnOff Rectangle
  | Toggle Rectangle
  deriving (Show)

row :: Int -> Row
row x = [((x, y), Off) | y <- [0 .. 999]]

grid :: Grid
grid = [row y | y <- [0 .. 999]]

execute :: Instruction -> Grid -> Grid
execute (TurnOn r) g = map (map (\l -> if inRectangle r l then turnOn l else l)) g
execute (TurnOff r) g = map (map (\l -> if inRectangle r l then turnOff l else l)) g
execute (Toggle r) g = map (map (\l -> if inRectangle r l then toggle l else l)) g

inRectangle :: Rectangle -> Light -> Bool
inRectangle (Rectangle (x1, y1) (x2, y2)) ((l1, l2), _) = l1 >= x1 && l1 <= x2 && l2 >= y1 && l2 <= y2

countLit :: Grid -> Int
countLit = length . filter (\l -> snd l == On) . concat

turnOn :: Light -> Light
turnOn (p, _) = (p, On)

turnOff :: Light -> Light
turnOff (p, _) = (p, Off)

toggle :: Light -> Light
toggle (p, On) = (p, Off)
toggle (p, Off) = (p, On)

createInstruction :: String -> Instruction
createInstruction s
  | startsWith s "turn on" = createTurnOn s
  | startsWith s "turn off" = createTurnOff s
  | startsWith s "toggle" = createToggle s
  | otherwise = error "instruction not recognized"

startsWith :: String -> String -> Bool
startsWith xs ys = take (length ys) xs == ys

createTurnOn :: String -> Instruction
createTurnOn s = TurnOn (Rectangle (x1, y1) (x2, y2))
  where
    w = words s
    x1 = read (fst (break (== ',') (w !! 2))) :: Int
    x2 = read (fst (break (== ',') (w !! 4))) :: Int
    y1 = read (tail (snd (break (== ',') (w !! 2)))) :: Int
    y2 = read (tail (snd (break (== ',') (w !! 4)))) :: Int

createTurnOff :: String -> Instruction
createTurnOff s = TurnOff (Rectangle (x1, y1) (x2, y2))
  where
    w = words s
    x1 = read (fst (break (== ',') (w !! 2))) :: Int
    x2 = read (fst (break (== ',') (w !! 4))) :: Int
    y1 = read (tail (snd (break (== ',') (w !! 2)))) :: Int
    y2 = read (tail (snd (break (== ',') (w !! 4)))) :: Int

createToggle :: String -> Instruction
createToggle s = Toggle (Rectangle (x1, y1) (x2, y2))
  where
    w = words s
    x1 = read (fst (break (== ',') (w !! 1))) :: Int
    x2 = read (fst (break (== ',') (w !! 3))) :: Int
    y1 = read (tail (snd (break (== ',') (w !! 1)))) :: Int
    y2 = read (tail (snd (break (== ',') (w !! 3)))) :: Int
