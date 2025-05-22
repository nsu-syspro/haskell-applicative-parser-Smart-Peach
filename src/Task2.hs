{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import ParserCombinators (choice, char, string)
import Task1

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--
date :: Parser Date
date = choice [dateUSFormat, dateDotFormat, dateHyphenFormat]

dateUSFormat :: Parser Date
dateUSFormat = do
                m <- monthUS <* char ' '
                d <- dayUS <* char ' '
                Date d m <$> year

dateDotFormat :: Parser  Date
dateDotFormat = dateCharFormat '.'

dateHyphenFormat :: Parser Date
dateHyphenFormat = dateCharFormat '-'

dateCharFormat :: Char -> Parser Date
dateCharFormat ch = Date <$> 
                        (day <* char ch) <*> 
                        (month <* char ch )<*> 
                        year

day :: Parser Day
day = Day . read <$> choice (map string days)
  where
    days = ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "26", "27", "28", "29", "30", "31"]

dayUS :: Parser Day
dayUS = Day . read <$> choice (map string days)
  where
    days = ["10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "26", "27", "28", "29", "30", "31", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

month :: Parser Month
month = Month . read <$> choice (map string days)
  where
    days = ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"]

monthUS :: Parser Month
monthUS =  Month . monthToNum <$> choice (map string months)
  where
    months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

year :: Parser Year
year = Year <$> fmap fromIntegral nat

monthToNum :: String -> Int
monthToNum s = case s of
  "Jan" -> 1
  "Feb" -> 2
  "Mar" -> 3
  "Apr" -> 4
  "May" -> 5
  "Jun" -> 6
  "Jul" -> 7
  "Aug" -> 8
  "Sep" -> 9
  "Oct" -> 10
  "Nov" -> 11
  "Dec" -> 12
  _     -> error "Invalid value for a Month value"
