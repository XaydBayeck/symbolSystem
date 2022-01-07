module SymbolSystem.ParserGenerator where

import Control.Applicative (Applicative (liftA2), (<|>))
import SymbolSystem.ExprData (StrictList, fromList)
import SymbolSystem.ParserMonad
  ( ParseResult (Err, Res),
    Parser (Prs),
  )

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

(|>) :: Parser a -> Parser a -> Parser a
(Prs p) |> (Prs g) = Prs $ \i0 -> case p i0 of
  (Res _ _) -> g i0
  e@(Err _) -> e