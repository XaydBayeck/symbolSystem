module SymbolSystem.ExprParser where

import SymbolSystem.ExprParserMonad

is :: Char -> Parser Char
is c = satisfy (c ==)
