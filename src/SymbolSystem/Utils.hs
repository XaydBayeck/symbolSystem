-- | utils
module SymbolSystem.Utils where

-- | `x |> h |> g |> f` 等价于 `f (g (h x))`
(|>) :: a -> (a -> b) -> b
x |> f = f x
