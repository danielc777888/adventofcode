import Data.List
import Data.Maybe
import Data.Char
import Text.Read
import Text.Regex.TDFA

main :: IO()
main = interact solve

data Passport = Passport BirthYear IssueYear ExpirationYear Height HairColor EyeColor PassportID CountryID deriving (Show)
type BirthYear = Maybe Int
type IssueYear = Maybe Int
type ExpirationYear = Maybe Int
type Height = Maybe Int
type HairColor = Maybe String
type EyeColor = Maybe String
type PassportID = Maybe Int
type CountryID = Maybe Int

noStr :: String
noStr = ""

solve :: String -> String
solve = show . length . filter valid . map passport . passportLines

passportLines :: String -> [[String]]
passportLines = passportLines' . map words .  lines

passportLines' :: [[String]] -> [[String]]
passportLines' [] = []
passportLines' xss = [concat (takeWhile (/= []) xss)] ++ passportLines' (if null xss' then [] else tail xss')
                     where xss' = dropWhile (/= []) xss

passport :: [String] -> Passport
passport xs = Passport byr iyr eyr hgt hcl elc pid cid
  where byr = birthYear (value xs "byr")
        iyr = issueYear (value xs "iyr")
        eyr = expirationYear  (value xs "eyr")
        hgt = height (value xs "hgt")
        hcl = hairColor (value xs "hcl")
        elc = eyeColor (value xs "ecl")
        pid = passportID (value xs "pid")
        cid = countryID (value xs "cid")

valid :: Passport -> Bool
valid (Passport (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) _) = True
valid _ = False

value :: [String] -> String -> String
value xs f = if null xs' then noStr else tail (snd (break (==':') (head xs')))
  where xs' = filter (f `isPrefixOf`) xs

birthYear :: String -> BirthYear
birthYear xs
  | xs =~ "^[0-9]{4}$"::Bool = if x >= 1920 && x <= 2002 then Just x else Nothing
  | otherwise = Nothing
  where x = read xs::Int
  
issueYear :: String -> IssueYear
issueYear xs
  | xs =~ "^[0-9]{4}$"::Bool = if x >= 2010 && x <= 2020 then Just x else Nothing
  | otherwise = Nothing
  where x = read xs::Int

expirationYear :: String -> ExpirationYear
expirationYear xs
  | xs =~ "^[0-9]{4}$"::Bool = if x >= 2020 && x <= 2030 then Just x else Nothing
  | otherwise = Nothing
  where x = read xs::Int

height :: String -> Height
height xs
  | xs =~ "^[0-9]{2,3}(cm|in)$"::Bool = if valid then Just x else Nothing 
  | otherwise = Nothing
  where x = read (takeWhile isDigit xs)::Int
        u = snd (break isLetter xs)
        valid = if u == "cm" then x >= 150 && x <= 193 else x >= 59 && x <= 76

hairColor :: String -> HairColor
hairColor xs = if xs =~ "^#[0-9a-f]{6}$"::Bool then Just xs else Nothing

eyeColor :: String -> EyeColor
eyeColor xs = if xs =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"::Bool then Just xs else Nothing

passportID :: String -> PassportID
passportID xs = if xs =~ "^[0-9]{9}$"::Bool then Just (read xs::Int) else Nothing

countryID :: String -> CountryID
countryID xs = if xs =~ "^[0-9]+$"::Bool then Just (read xs::Int) else Nothing
