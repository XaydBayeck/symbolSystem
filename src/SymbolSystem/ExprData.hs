-- | The struct of data Expression and datas about it
module SymbolSystem.ExprData where

newtype Symbol = Sym String deriving (Eq)

instance Show Symbol where
  show (Sym s) = s

newtype Operate = Op String deriving (Eq)

instance Show Operate where
  show (Op op) = op

data Expression = Expr {op :: Operate, exprs :: Exprs} | Variable {var :: Symbol}

{-- This List has at least one element --}
data StrictList a = Single a | Cons a (StrictList a)

instance Show a => Show (StrictList a) where
  show (Single x) = show x
  show (Cons x xs) = show x ++ " : " ++ show xs

(.>) :: a -> StrictList a -> StrictList a
(.>) = Cons

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

fromList :: [a] -> StrictList a
fromList [] = error "list cannot be empty"
fromList [x] = Single x
fromList (x : xs) = Cons x $ fromList xs
