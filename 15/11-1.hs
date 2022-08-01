main :: IO ()
main = interact . solve

type Password = String

solve :: String -> String
solve = until valid increment

increment :: Password -> Password
increment = undefined

valid :: Password -> Bool
valid = undefined
