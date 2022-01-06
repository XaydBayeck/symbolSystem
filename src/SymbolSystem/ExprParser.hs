module SymbolSystem.ExprParser where

import Control.Applicative (liftA2)
import Data.Char (isAlpha, isDigit, isSpace)
import SymbolSystem.ExprParserMonad

is :: Char -> Parser Char
is c = satisfy (c ==)

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

parseList :: Parser a -> Parser [a]
parseList pa = parseList1 pa <|> pure []

parseList1 :: Parser a -> Parser [a]
parseList1 pa = liftA2 (:) pa (parseList pa)

many :: Parser a -> Parser [a]
many = parseList

anyString :: Parser String
anyString = parseList anyChar

seqParser :: [Parser a] -> Parser [a]
{--若没有解析器，返回一个返回`[]`但不进行解析的解析器`pure []`--}
seqParser [] = pure []
seqParser (p : ps) =
  p >>= \x ->
    seqParser ps >>= \xs ->
      pure $ x : xs

string :: String -> Parser String
string s = seqParser $ map is s

word :: Parser String
word = many $ satisfy isAlpha

between :: Parser b -> Parser a -> Parser c -> Parser a
between pl pm pr = pl *> pm <* pr

sepBy :: Parser b -> Parser a -> Parser [a]
sepBy pb pa =
  ( pa >>= \h ->
      many (pb *> pa) >>= \t ->
        pure $ h : t
  )
    <|> pure []
