module SymbolSystem.ExprParser where

import Control.Applicative (liftA2, (<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import SymbolSystem.ExprData
import SymbolSystem.ExprParserMonad

{-- Parser generater --}

many :: Parser a -> Parser [a]
many pa = many1 pa <|> pure []

many1 :: Parser a -> Parser [a]
many1 pa = liftA2 (:) pa (many pa)

seqParser :: [Parser a] -> Parser [a]
{--若没有解析器，返回一个返回`[]`但不进行解析的解析器`pure []`--}
seqParser [] = pure []
seqParser (p : ps) = do
  x <- p
  xs <- seqParser ps
  return $ x : xs

between :: Parser b -> Parser a -> Parser c -> Parser a
between pl pm pr = pl *> pm <* pr

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb =
  ( do
      h <- pa
      t <- many (pb *> pa)
      return $ h : t
  )
    <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = do
  h <- pa
  t <- many (pb *> pa)
  return $ h : t

sepBy1' :: Parser a -> Parser b -> Parser (StrictList a)
sepBy1' pa pb = fromList <$> sepBy1 pa pb

{-- String Parser --}

is :: Char -> Parser Char
is c = satisfy (c ==)

isnt :: Char -> Parser Char
isnt c = satisfy (c /=)

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

anyString :: Parser String
anyString = many character

string :: String -> Parser String
string s = seqParser $ map is s

word :: Parser String
word = many $ satisfy isAlpha

unspace :: Parser Char
unspace = satisfy $ not . (`elem` [' ', '\t'])

-- | Character is not `' ', '/\t', '(', ')', '{', '}', '[', ']'`
unspecial :: Parser Char
unspecial = satisfy $ not . (`elem` [' ', '\t', '(', ')', '{', '}', '[', ']'])

{-- Expression Parser --}

opP :: Parser Operate
opP = Op <$> between (is '(') (many unspecial) space

variable :: Parser Symbol
variable = Sym <$> many unspecial

exprP :: Parser Expression
exprP = Variable <$> variable <|> Expr <$> opP <*> exprsP

exprsP :: Parser (StrictList Expression)
exprsP = exprP `sepBy1'` many space
