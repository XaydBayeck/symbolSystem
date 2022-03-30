module SymbolSystem.ExprParser
  ( opP,
    variable,
    exprP,
    exprsP,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Char
import SymbolSystem.ExprData
  ( Expression (Expr, Variable),
    Operate (..),
    StrictList,
    Symbol (..),
  )
import SymbolSystem.ParserGenerator (between, many, many1, sepBy1', times)
import SymbolSystem.ParserMonad (Parser)
import SymbolSystem.TextParser (is, satisfy, space, unspecial, varP)
import SymbolSystem.Utils ((<++>))

{-- Expression Parser --}

opP :: Parser Operate
opP = Op <$> between (is '(') (many1 unspecial) space

variable :: Parser Symbol
variable = Sym <$> times 1 (satisfy $ not . isNumber) <++> many unspecial

exprP :: Parser Expression
exprP = (Expr <$> opP <*> exprsP <* is ')') <|> (Variable <$> variable)

exprsP :: Parser (StrictList Expression)
exprsP = exprP `sepBy1'` many1 space
