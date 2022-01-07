{-# LANGUAGE LambdaCase #-}

module SymbolSystem.TextParser where

import Data.Char (isAlpha, isDigit, isSpace)
import SymbolSystem.ParserGenerator (many, seqParser)
import SymbolSystem.ParserMonad
  ( ParseErr (UnexpectedChar, UnexpectedEof),
    ParseResult (Err, Res),
    Parser (Prs),
  )

{-- String Parser --}

character :: Parser Char
character = Prs $ \case
  (c : cs) -> Res cs c
  _ -> Err UnexpectedEof

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = (>>=) character $ \a ->
  if f a
    then pure a
    else Prs $ \_ -> Err $ UnexpectedChar a

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