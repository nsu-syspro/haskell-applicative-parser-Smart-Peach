{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , option
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where

import Control.Applicative
import Control.Monad (ap)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse (Parser p) s = p $ Position 0 s

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case parse p s of
                    Parsed a _ -> Just a
                    _          -> Nothing

instance Functor Parsed where
  -- fmap f (Parsed a input) = Parsed (f a) input
  -- fmap _ (Failed err) = Failed err
  fmap f p = f <$> p

instance Applicative Parsed where -- Seem stupid
  pure i = Parsed i undefined

  (<*>) (Parsed f _) (Parsed a s) = Parsed (f a) s -- seems wrong
  (<*>) _ (Failed err) = Failed err 
  (<*>) (Failed err) _ = Failed err 

instance Alternative Parsed where
  empty = Failed []
  (<|>) (Parsed a s) _ = Parsed a s
  (<|>) _ (Parsed a s) = Parsed a s
  (<|>) (Failed err1) (Failed err2) = Failed $ err1 ++ err2

instance Functor Parser where
  -- fmap f (Parser p) = Parser $ fmap f . p
  fmap f p = pure f <*> p

instance Monad Parser where 
  (Parser p) >>= f = Parser $ \input -> 
    case p input of
      Parsed val input' -> runParser (f val) input'
      Failed err     -> Failed err

instance Applicative Parser where
  pure value = Parser(Parsed value)
  (<*>) = ap -- is it considered cheating?

instance Alternative Parser where
  empty = Parser (const (Failed []))
  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  (<|>) (Parser a) (Parser b) = Parser $ \s -> a s <|> b s

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
  Position pos (c : rest)
    | predicate c -> Parsed c (Position (pos + 1) rest)
  Position pos (c : _) -> Failed [Position pos (Unexpected c)]
  Position pos [] -> Failed [Position pos EndOfInput]
                                    
option :: a -> Parser a -> Parser a
option val p = p <|> return val
