
main :: IO()
main = interact solve

data Rule = Const Char
          | Rule [Int]
          | Or [Int] [Int]
          
type Message = String

solve :: String -> String
solve = show . length . filter id . matches . parse . lines

parse :: [String] -> (Rule, [Message])
parse = undefined

rules :: [String] -> Rule
rules = undefined

messages :: [String] -> [Message]
messages = undefined

matches :: (Rule, [Message]) -> [Bool]
matches = undefined

match :: Message -> Rule -> Bool
match = undefined
