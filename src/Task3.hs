{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import Data.Char (toLower, digitToInt, isDigit)
import Data.List (intercalate)

import ParserCombinators(char, string, choice, sepBy, ws)
import Data.Functor (($>))

import Control.Applicative ((<|>), Alternative (many))
import Prelude hiding (exp, exponent)
import GHC.Char (chr)
import GHC.Unicode (isHexDigit)
-- | JSON representation
--
-- See <https://www.json.org>
--

data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--
json :: Parser JValue
json = choice [jObject, jNumber, jBool, jNull, jArray, jString]

jObject :: Parser JValue
jObject = do
          _ <- ws *> char '{' *> ws
          pairs <- sepBy pair (char ',' *> ws)
          _ <- ws <* char '}'
          return (JObject pairs)

jArray :: Parser JValue
jArray = do
          _ <- ws *> char '[' *> ws
          values <- sepBy json (ws *> char ',' <* ws)
          _ <- ws <* char ']' <* ws
          return (JArray values)

jNumber :: Parser JValue
jNumber = do
            int <- integer
            fract <- fraction
            exp <- exponent
            return (JNumber $ read (int ++ fract ++ exp))

jString :: Parser JValue
jString = JString <$> ordString

jBool :: Parser JValue
jBool = JBool <$> choice [string "true" $> True, string "false" $> False]

jNull :: Parser JValue
jNull = JNull <$ string "null"

pair :: Parser (String, JValue)
pair = do
        k <- ws *> ordString <* ws <* char ':' <* ws
        v <- json <* ws
        return (k, v)

integer :: Parser String
integer = do
            s <- option "" $ string "-"
            digits <- ((++) . (:[]) <$> onenine) <*> many digit <|> (:[]) <$> digit
            return (s ++ digits)

fraction :: Parser String
fraction = option "" $ ((++) . (:[]) <$> char '.') <*> many digit

exponent :: Parser String
exponent = option "" $ do
                        e <- char 'e' <|> char 'E'
                        s <- sign
                        digits <- many digit
                        return (e : s ++ digits)

sign :: Parser String
sign = option "" $ string "-" <|> string "+"

digit :: Parser Char
digit = satisfy isDigit

onenine :: Parser Char
onenine = satisfy (\c -> isDigit c && c /= '0')

ordString :: Parser String
ordString = do
    _ <- char '"'
    chars <- concat <$> many characters
    _ <- char '"'
    return chars

characters :: Parser String
characters = ordinaryChar <|> escape

ordinaryChar :: Parser String
ordinaryChar = (:[]) <$> satisfy (\c -> c /= '"' && c /= '\\')

escape :: Parser String
escape =  char '\\' >> choice
  [ string "\"" >> return "\\\""
  , string "\\" >> return "\\\\"
  , string "/"  >> return "\\/"
  , string "b"  >> return "\\b"
  , string "f"  >> return "\\f"
  , string "n"  >> return "\\n"
  , string "r"  >> return "\\r"
  , string "t"  >> return "\\t"
  , string "u"  >> unicode
  ]

unicode :: Parser String
unicode = do
  d1 <- hex
  d2 <- hex
  d3 <- hex
  d4 <- hex
  let code = d1 * 4096 + d2 * 256 + d3 * 16 + d4
  return [chr code]

hex :: Parser Int
hex = do
  c <- satisfy isHexDigit
  case digitToInt c of
    n -> return n

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
