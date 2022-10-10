import Data.List

main :: IO ()
main = interact solve

data Password = Password BirthYear IssueYear ExpirationYear Height HairColor EyeColor PassportID CountryID deriving (Show)

type BirthYear = Int

type IssueYear = Int

type ExpirationYear = Int

type Height = String

type HairColor = String

type EyeColor = String

type PassportID = String

type CountryID = Int

type Field = String

noStr :: String
noStr = ""

noInt :: Int
noInt = -1

solve :: String -> String
solve = show . length . filter valid . map password . passwordLines

-- solve = undefined

passwordLines :: String -> [[String]]
passwordLines = passwordLines' . map words . lines

passwordLines' :: [[String]] -> [[String]]
passwordLines' [] = []
passwordLines' xss = [concat (takeWhile (/= []) xss)] ++ passwordLines' (if null xss' then [] else tail xss')
  where
    xss' = dropWhile (/= []) xss

password :: [String] -> Password
password xs = Password birthYear issueYear expirationYear height hairColor eyeColor passportID countryID
  where
    birthYear = getInt xs "byr"
    issueYear = getInt xs "iyr"
    expirationYear = getInt xs "eyr"
    height = getStr xs "hgt"
    hairColor = getStr xs "hcl"
    eyeColor = getStr xs "ecl"
    passportID = getStr xs "pid"
    countryID = getInt xs "cid"

valid :: Password -> Bool
valid (Password byr iyr eyr hgt hcl ecl pid cid)
  | byr /= noInt && iyr /= noInt && eyr /= noInt && hgt /= noStr && hcl /= noStr && ecl /= noStr && pid /= noStr = True
  | otherwise = False

getStr :: [String] -> Field -> String
getStr xs f = if null xs' then noStr else tail (snd (break (== ':') (head xs')))
  where
    xs' = filter (f `isPrefixOf`) xs

getInt :: [String] -> Field -> Int
getInt xs f = if null xs' then noInt else read (tail (snd (break (== ':') (head xs')))) :: Int
  where
    xs' = filter (f `isPrefixOf`) xs
