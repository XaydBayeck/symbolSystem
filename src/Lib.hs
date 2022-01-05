module Lib where

import SystemSymbol.ExprParser ()

newtype Symbol = Sym String deriving (Eq)

instance Show Symbol where
  show (Sym s) = s

newtype Operate = Op String deriving (Eq)

instance Show Operate where
  show (Op op) = op

data Expression = Expr Operate Exprs | Variable Symbol

{-- This List has at least one element --}
data StrictList a = Single a | Cons a (StrictList a)

type Exprs = StrictList Expression

instance Show Expression where
  show (Variable v) = show v -- print v
  show (Expr op es) = "(" ++ show op ++ " " ++ exprs ++ ")" -- print (op expr1 expr2 ...)
    where
      exprs = case es of
        Single e -> show e
        Cons e es -> helper (show e) es
          where
            helper res (Single e) = res ++ " " ++ show e
            helper res (Cons e es) = helper (res ++ " " ++ show e) es

data Result t e = Ok t | Err e

parseComma :: String -> Maybe (String, String, String)
parseComma = commaParser "" "" 0
  where
    commaParser ps s n "" = case (ps, s) of
      ("", "") -> Nothing
      (ps, s) -> Just (ps, s, "")
    commaParser ps s 0 (b : str)
      | b == '(' = commaParser ps s 1 str
      | otherwise = commaParser (ps ++ b : "") s 0 str
    commaParser ps s 1 (')' : st) = Just (ps, s, st)
    commaParser ps s n (b : str)
      | b == '(' = commaParser ps (s ++ "(") (n + 1) str
      | b == ')' = commaParser ps (s ++ ")") (n - 1) str
      | otherwise = commaParser ps (s ++ b : "") n str

parseSpace :: String -> Maybe (String, String)
parseSpace "" = Nothing
parseSpace s = brackParse "" s
  where
    brackParse s "" = if s == "" then Nothing else Just ("", s)
    brackParse s (c : str)
      | c == ' ' = Just (s, str)
      | otherwise = brackParse (s ++ c : "") str

parseExpr :: String -> Maybe Expression
parseExpr es = case parseComma es of
  Nothing -> Nothing
  Just (v, "", "") -> Just $ Variable $ Sym v
  Just (_, epr, _) -> case parseSpace epr of
    Nothing -> Nothing
    Just (op, exprs) -> case parseExprs exprs of
      Nothing -> Nothing
      Just es -> Just $ Expr (Op op) es

parseExprs :: String -> Maybe (StrictList Expression)
parseExprs eps = case parseSpace eps of
  Nothing -> Nothing
  Just ("", e) -> case parseExpr e of
    Nothing -> Nothing
    Just e -> Just $ Single e
  Just (e, es) -> case parseExpr e of
    Nothing -> parseExprs es
    Just e -> case parseExprs es of
      Nothing -> Just $ Single e
      Just es -> Just $ Cons e es
