module SymbolSystem.ParserGenerator where

import Control.Applicative (Applicative, (<|>))
import SymbolSystem.ExprData (StrictList, fromList)
import SymbolSystem.ParserMonad
  ( ParseResult (Err, Res),
    Parser (Prs),
  )

{-- Parser generater --}

many :: Parser a -> Parser [a]
many pa = many1 pa <|> pure []

many1 :: Parser a -> Parser [a]
-- many1 pa = liftA2 (:) pa (many pa)
many1 pa = (:) <$> pa <*> many pa

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

times :: Integer -> Parser a -> Parser [a]
times n pa
  | n < 0 = error "n cannot little than 0"
  | n == 0 = pure []
  | otherwise = (:) <$> pa <*> times (n - 1) pa
