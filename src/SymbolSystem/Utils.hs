-- | utils
module SymbolSystem.Utils where

-- | `x |> h |> g |> f` 等价于 `f (g (h x))`
(|>) :: a -> (a -> b) -> b
x |> f = f x

(<+>) :: Monad m => m [a] -> m [a] -> m [a]
ma <+> mb = do
  x1 <- ma
  x2 <- mb
  return $ x1 ++ x2
