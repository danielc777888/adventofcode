import Data.List
import Data.Maybe
import Data.Char
import Text.Read
import Text.Regex.TDFA

main :: IO()
main = interact solve

data Password = Password BirthYear IssueYear ExpirationYear Height HairColor EyeColor PassportID CountryID deriving (Show)
type BirthYear = Maybe Int
type IssueYear = Maybe Int
type ExpirationYear = Maybe Int
type Height = Maybe Int
type HairColor = Maybe String
type EyeColor = Maybe String
type PassportID = Maybe Int
type CountryID = Maybe Int

type Field = String

noStr :: String
noStr = ""

solve :: String -> String
solve = show . length . filter valid . map password . passwordLines

passwordLines :: String -> [[String]]
passwordLines = passwordLines' . map words .  lines

passwordLines' :: [[String]] -> [[String]]
passwordLines' [] = []
passwordLines' xss = [concat (takeWhile (/= []) xss)] ++ passwordLines' (if null xss' then [] else tail xss' )
                     where xss' = dropWhile (/= []) xss

password :: [String] -> Password
password xs = Password byr iyr eyr hgt hcl elc pid cid
  where byr = birthYear (value xs "byr")
        iyr = issueYear (value xs "iyr")
        eyr = expirationYear  (value xs "eyr")
        hgt = height (value xs "hgt")
        hcl = hairColor (value xs "hcl")
        elc = eyeColor (value xs "ecl")
        pid = passportID (value xs "pid")
        cid = countryID (value xs "cid")

valid :: Password -> Bool
valid (Password byr iyr eyr hgt hcl ecl pid cid)
       | isJust byr && isJust iyr && isJust eyr && isJust hgt && isJust hcl && isJust ecl && isJust pid = True
       | otherwise = False

value :: [String] -> Field -> String
value xs f = if null xs' then noStr else tail (snd (break (==':') (head xs')))
  where xs' = filter (f `isPrefixOf`) xs

birthYear :: String -> BirthYear
birthYear xs
  | xs =~ "^[0-9]{4}$"::Bool = if x >= 1920 && x <= 2002 then Just x else Nothing
  | otherwise = Nothing
  where x = read  xs::Int
  
issueYear :: String -> IssueYear
issueYear xs
  | xs =~ "^[0-9]{4}$"::Bool = if x >= 2010 && x <= 2020 then Just x else Nothing
  | otherwise = Nothing
  where x = read  xs::Int

expirationYear :: String -> ExpirationYear
expirationYear xs
  | xs =~ "^[0-9]{4}$"::Bool = if x >= 2020 && x <= 2030 then Just x else Nothing
  | otherwise = Nothing
  where x = read  xs::Int

height :: String -> Height
height xs
  | xs =~ "^[0-9]{2,3}(cm|in)$"::Bool = if valid then Just x else Nothing 
  | otherwise = Nothing
  where x = read (takeWhile isDigit xs)::Int
        u = snd (break isLetter xs)
        valid = if u == "cm" then x >= 150 && x <= 193 else x >= 59 && x <= 76


hairColor :: String -> HairColor
hairColor xs
  | xs =~ "^#[0-9a-f]{6}$"::Bool = Just xs
  | otherwise = Nothing

eyeColor :: String -> EyeColor
eyeColor xs
  | xs =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"::Bool = Just xs
  | otherwise = Nothing

passportID :: String -> PassportID
passportID xs
  | xs =~ "^[0-9]{9}$"::Bool = Just (read xs::Int)
  | otherwise = Nothing

countryID :: String -> CountryID
countryID xs
  | xs =~ "^[0-9]+$"::Bool = Just (read xs::Int)
  | otherwise = Nothing
