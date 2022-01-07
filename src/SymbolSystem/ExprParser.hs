module SymbolSystem.ExprParser
  ( opP,
    variable,
    exprP,
    exprsP,
  )
where

import Control.Applicative (Alternative ((<|>)))
import SymbolSystem.ExprData
  ( Expression (Expr, Variable),
    Operate (..),
    StrictList,
    Symbol (..),
  )
import SymbolSystem.ParserGenerator (between, many1, sepBy1')
import SymbolSystem.ParserMonad (Parser)
import SymbolSystem.TextParser (is, space, unspecial)

{-- Expression Parser --}

opP :: Parser Operate
opP = Op <$> between (is '(') (many1 unspecial) space

variable :: Parser Symbol
variable = Sym <$> many1 unspecial

exprP :: Parser Expression
exprP = (Variable <$> variable) <|> (Expr <$> opP <*> exprsP <* is ')')

exprsP :: Parser (StrictList Expression)
exprsP = exprP `sepBy1'` many1 space
